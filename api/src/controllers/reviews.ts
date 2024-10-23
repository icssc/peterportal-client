/**
 @module ReviewRoute
*/

import { verifyCaptcha } from '../helpers/recaptcha';
import Review from '../models/review';
import Vote from '../models/vote';
import Report from '../models/report';
import mongoose from 'mongoose';
import { adminProcedure, publicProcedure, router, userProcedure } from '../helpers/trpc';
import { z } from 'zod';
import { editReviewSubmission, featuredQuery, ReviewData, reviewSubmission } from '@peterportal/types';
import { TRPCError } from '@trpc/server';

interface ReviewFilter {
  courseID?: string;
  professorID?: string;
  userID?: string;
  _id?: mongoose.Types.ObjectId;
  verified?: boolean;
}

interface VoteData {
  _id?: string;
  userID: string;
  reviewID: string;
  score: number;
}

async function userWroteReview(userID: string | undefined, reviewID: string) {
  if (!userID) {
    return false;
  }

  return await Review.exists({ _id: reviewID, userID: userID });
}

const reviewsRouter = router({
  /**
   * Query reviews
   */
  get: publicProcedure
    .input(
      z.object({
        courseID: z.string().optional(),
        professorID: z.string().optional(),
        verified: z.boolean().optional(),
        userID: z.string().optional(),
        reviewID: z.string().optional(),
      }),
    )
    .query(async ({ input, ctx }) => {
      const { courseID, professorID, userID, reviewID, verified } = input;

      const query: ReviewFilter = {
        courseID,
        professorID,
        userID,
        _id: reviewID ? new mongoose.Types.ObjectId(reviewID) : undefined,
        verified,
      };

      // remove null params
      for (const param in query) {
        if (query[param as keyof ReviewFilter] === null || query[param as keyof ReviewFilter] === undefined) {
          delete query[param as keyof ReviewFilter];
        }
      }

      const pipeline = [
        {
          $match: query,
        },
        {
          $addFields: {
            _id: {
              $toString: '$_id',
            },
          },
        },
        {
          $lookup: {
            from: 'votes',
            let: {
              cmpUserID: ctx.session.passport?.user.id,
              cmpReviewID: '$_id',
            },
            pipeline: [
              {
                $match: {
                  $expr: {
                    $and: [
                      {
                        $eq: ['$$cmpUserID', '$userID'],
                      },
                      {
                        $eq: ['$$cmpReviewID', '$reviewID'],
                      },
                    ],
                  },
                },
              },
            ],
            as: 'userVote',
          },
        },
        {
          $addFields: {
            userVote: {
              $cond: {
                if: {
                  $ne: ['$userVote', []],
                },
                then: {
                  $getField: {
                    field: 'score',
                    input: {
                      $first: '$userVote',
                    },
                  },
                },
                else: 0,
              },
            },
          },
        },
      ];

      const reviews = await Review.aggregate<ReviewData>(pipeline);
      if (reviews) {
        return reviews;
      } else {
        return [];
      }
    }),

  /**
   * Add a review
   */
  add: userProcedure.input(reviewSubmission).mutation(async ({ input, ctx }) => {
    // check if user is trusted
    const verifiedCount = await Review.find({
      userID: ctx.session.passport!.user.id,
      verified: true,
    })
      .countDocuments()
      .exec();

    const review = {
      ...input,
      userDisplay: input.userDisplay === 'Anonymous Peter' ? 'Anonymous Peter' : ctx.session.passport!.user.name,
      userID: ctx.session.passport!.user.id,
      verified: verifiedCount >= 3, // auto-verify if use has 3+ verified reviews
    };

    //check if review already exists for same professor, course, and user
    const query: ReviewFilter = {
      courseID: input.courseID,
      professorID: input.professorID,
      userID: ctx.session.passport?.user.id,
    };

    const reviews = await Review.find(query);
    if (reviews?.length > 0)
      throw new TRPCError({ code: 'BAD_REQUEST', message: 'You have already reviewed this professor and course!' });

    // Verify the captcha
    const verifyResponse = await verifyCaptcha(review);
    if (!verifyResponse?.success) throw new TRPCError({ code: 'BAD_REQUEST', message: 'ReCAPTCHA token is invalid' });
    delete review.captchaToken; // so it doesn't get stored in DB

    // add review to mongo
    return (await new Review(review).save()) as unknown as ReviewData;
  }),

  /**
   * Delete a review (user can delete their own or admin can delete any through reports)
   */
  delete: userProcedure.input(z.object({ id: z.string() })).mutation(async ({ input, ctx }) => {
    if (ctx.session.passport!.isAdmin || (await userWroteReview(ctx.session.passport!.user.id, input.id))) {
      await Review.deleteOne({ _id: input.id });
      // delete all votes and reports associated with review
      await Vote.deleteMany({ reviewID: input.id });
      await Report.deleteMany({ reviewID: input.id });
      return true;
    } else {
      throw new TRPCError({ code: 'UNAUTHORIZED', message: 'Must be an admin or review author to delete reviews!' });
    }
  }),

  /**
   * Vote on a review
   */
  vote: userProcedure.input(z.object({ id: z.string(), upvote: z.boolean() })).mutation(async ({ input, ctx }) => {
    //get id and delta score from initial vote
    const id = input.id;
    let deltaScore = input.upvote ? 1 : -1;
    //query to search for a vote matching the same review and user
    const currentVotes = {
      userID: ctx.session.passport!.user.id,
      reviewID: id,
    };
    //either length 1 or 0 array(ideally) 0 if no existing vote, 1 if existing vote
    const existingVote = (await Vote.find(currentVotes)) as VoteData[];
    //check if there is an existing vote and it has the same vote as the previous vote
    if (existingVote.length != 0 && deltaScore == existingVote[0].score) {
      //remove the vote

      //delete the existing vote from the votes collection
      await Vote.deleteMany(currentVotes);
      //update the votes document with a lowered score
      await Review.updateOne({ _id: id }, { $inc: { score: -1 * deltaScore } });

      return { deltaScore: -1 * deltaScore };
    } else if (existingVote.length != 0 && deltaScore != existingVote[0].score) {
      //there is an existing vote but the vote was different
      deltaScore *= 2;
      //*2 to reverse the old vote and implement the new one
      await Review.updateOne({ _id: id }, { $inc: { score: deltaScore } });
      //override old vote with new data
      await Vote.updateOne({ _id: existingVote[0]._id }, { $set: { score: deltaScore / 2 } });

      return { deltaScore: deltaScore };
    } else {
      //no old vote, just add in new vote data
      await Review.updateOne({ _id: id }, { $inc: { score: deltaScore } });
      //sends in vote
      await new Vote({ userID: ctx.session.passport!.user.id, reviewID: id, score: deltaScore }).save();
      return { deltaScore: deltaScore };
    }
  }),

  verify: adminProcedure.input(z.object({ id: z.string() })).mutation(async ({ input }) => {
    return await Review.updateOne({ _id: input.id }, { verified: true });
  }),

  edit: userProcedure.input(editReviewSubmission).mutation(async ({ input, ctx }) => {
    if (!(await userWroteReview(ctx.session.passport!.user.id, input._id))) {
      throw new TRPCError({ code: 'UNAUTHORIZED', message: 'You are not the author of this review.' });
    }

    const { _id, ...updateWithoutId } = input;
    await Review.updateOne({ _id }, updateWithoutId);
    return input;
  }),

  /**
   * Get featured review for a course or professor
   */
  featured: publicProcedure.input(featuredQuery).query(async ({ input }) => {
    // search by professor or course field
    let field = '';
    if (input.type == 'course') {
      field = 'courseID';
    } else if (input.type == 'professor') {
      field = 'professorID';
    }

    // find first review with the highest score
    const review = await Review.findOne<ReviewData>({ [field]: input.id })
      .sort({ reviewContent: -1, score: -1, verified: -1 })
      .limit(1);

    return review;
  }),

  /**
   * Get avg ratings for a course's professors or a professor's courses
   */
  scores: publicProcedure
    .input(z.object({ type: z.enum(['course', 'professor']), id: z.string() }))
    .query(async ({ input }) => {
      // match filters all reviews with given field
      // group aggregates by field
      let matchField = '';
      let groupField = '';
      if (input.type == 'professor') {
        matchField = 'professorID';
        groupField = '$courseID';
      } else if (input.type == 'course') {
        matchField = 'courseID';
        groupField = '$professorID';
      }

      // execute aggregation on the reviews collection

      const array = await Review.aggregate([
        { $match: { [matchField]: input.id } },
        { $group: { _id: groupField, score: { $avg: '$rating' } } },
      ]);

      // rename id to name
      const results = array.map((v) => {
        return { name: v._id as string, score: v.score as number };
      });

      return results;
    }),
});

export default reviewsRouter;
