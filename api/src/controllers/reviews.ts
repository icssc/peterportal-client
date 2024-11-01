/**
 @module ReviewRoute
*/

import { verifyCaptcha } from '../helpers/recaptcha';
import { adminProcedure, publicProcedure, router, userProcedure } from '../helpers/trpc';
import { z } from 'zod';
import {
  anonymousName,
  editReviewSubmission,
  featuredQuery,
  FeaturedReviewData,
  ReviewData,
  reviewSubmission,
} from '@peterportal/types';
import { TRPCError } from '@trpc/server';
import { db } from '../db';
import { review, user, vote } from '../db/schema';
import { and, count, desc, eq, sql } from 'drizzle-orm';

async function userWroteReview(userId: number | undefined, reviewId: number) {
  if (!userId) {
    return false;
  }

  return (
    (
      await db
        .select({ count: count() })
        .from(review)
        .where(and(eq(review.id, reviewId), eq(review.userId, userId)))
    )[0].count > 0
  );
}

const reviewsRouter = router({
  /**
   * Query reviews
   */
  get: publicProcedure
    .input(
      z.object({
        courseId: z.string().optional(),
        professorId: z.string().optional(),
        verified: z.boolean().optional(),
        userId: z.number().optional(),
        reviewId: z.number().optional(),
      }),
    )
    .query(async ({ input, ctx }) => {
      const { courseId, professorId, userId, reviewId, verified } = input;

      const userVoteSubquery = db
        .select({ reviewId: vote.reviewId, userVote: vote.vote })
        .from(vote)
        .where(eq(vote.userId, ctx.session.userId!))
        .as('user_vote_query');
      const results = await db
        .select({
          review: review,
          score: sql`COALESCE(SUM(${vote.vote}), 0)`.mapWith(Number),
          userDisplay: user.name,
          userVote: sql`COALESCE(${userVoteSubquery.userVote}, 0)`.mapWith(Number),
        })
        .from(review)
        .where(
          and(
            ...[
              ...(courseId ? [eq(review.courseId, courseId)] : []),
              ...(professorId ? [eq(review.professorId, professorId)] : []),
              ...(userId ? [eq(review.userId, userId)] : []),
              ...(reviewId ? [eq(review.id, reviewId)] : []),
              ...(verified ? [eq(review.verified, verified)] : []),
            ],
          ),
        )
        .leftJoin(vote, eq(vote.reviewId, review.id))
        .leftJoin(user, eq(user.id, review.userId))
        .leftJoin(userVoteSubquery, eq(userVoteSubquery.reviewId, review.id))
        .groupBy(review.id, user.name, userVoteSubquery.userVote);

      if (results) {
        return results.map(({ review, score, userDisplay, userVote }) => ({
          ...review,
          createdAt: review.createdAt.toISOString(),
          updatedAt: review.updatedAt?.toISOString(),
          score,
          userDisplay: review.anonymous ? anonymousName : userDisplay!,
          userVote: userVote,
        })) as ReviewData[];
      } else {
        return [];
      }
    }),

  /**
   * Add a review
   */
  add: userProcedure.input(reviewSubmission).mutation(async ({ input, ctx }) => {
    const userId = ctx.session.userId!;
    // check if user is trusted
    const { verifiedCount } = (
      await db
        .select({ verifiedCount: count() })
        .from(review)
        .where(and(eq(review.userId, userId), eq(review.verified, true)))
    )[0];
    const reviewToAdd = {
      ...input,
      userId: userId,
      verified: verifiedCount >= 3, // auto-verify if use has 3+ verified reviews
    };

    //check if review already exists for same professor, course, and user (do this before verifying captcha)
    const existingReview = await db
      .select({ count: count() })
      .from(review)
      .where(
        and(eq(review.userId, userId), eq(review.courseId, input.courseId), eq(review.professorId, input.professorId)),
      );
    if (existingReview[0].count > 0) {
      throw new TRPCError({ code: 'BAD_REQUEST', message: 'You have already reviewed this professor and course!' });
    }

    // Verify the captcha
    const verifyResponse = await verifyCaptcha(reviewToAdd);
    if (!verifyResponse?.success) throw new TRPCError({ code: 'BAD_REQUEST', message: 'ReCAPTCHA token is invalid' });

    const addedReview = (await db.insert(review).values(reviewToAdd).returning())[0];
    return {
      ...addedReview,
      createdAt: addedReview.createdAt.toISOString(),
      updatedAt: addedReview.updatedAt?.toISOString(),
      userDisplay: input.anonymous ? anonymousName : ctx.session.passport!.user.name,
      score: 0,
      userVote: 0,
    } as ReviewData;
  }),

  /**
   * Delete a review (user can delete their own or admin can delete any through reports)
   */
  delete: userProcedure.input(z.object({ id: z.number() })).mutation(async ({ input, ctx }) => {
    if (ctx.session.passport!.isAdmin || (await userWroteReview(ctx.session.userId, input.id))) {
      await db.delete(review).where(eq(review.id, input.id));
      return true;
    } else {
      throw new TRPCError({ code: 'UNAUTHORIZED', message: 'Must be an admin or review author to delete reviews!' });
    }
  }),

  /**
   * Vote on a review
   */
  vote: userProcedure
    .input(z.object({ id: z.number(), vote: z.number().int().min(-1).max(1) }))
    .mutation(async ({ input, ctx }) => {
      if (input.vote === 0) {
        await db.delete(vote).where(and(eq(vote.userId, ctx.session.userId!), eq(vote.reviewId, input.id)));
        return true;
      }

      await db
        .insert(vote)
        .values({ userId: ctx.session.userId!, reviewId: input.id, vote: input.vote })
        .onConflictDoUpdate({ target: [vote.userId, vote.reviewId], set: { vote: input.vote } });
    }),

  verify: adminProcedure.input(z.object({ id: z.number() })).mutation(async ({ input }) => {
    await db.update(review).set({ verified: true }).where(eq(review.id, input.id));
    return true;
  }),

  edit: userProcedure.input(editReviewSubmission).mutation(async ({ input, ctx }) => {
    if (!(await userWroteReview(ctx.session.userId, input.id))) {
      throw new TRPCError({ code: 'UNAUTHORIZED', message: 'You are not the author of this review.' });
    }

    const { id, ...updateWithoutId } = input;
    await db.update(review).set(updateWithoutId).where(eq(review.id, id));
    return true;
  }),

  /**
   * Get featured review for a course or professor
   */
  featured: publicProcedure.input(featuredQuery).query(async ({ input }) => {
    const voteSubQuery = db
      .select({ reviewId: vote.reviewId, score: sql`sum(${vote.vote})`.mapWith(Number).as('score') })
      .from(vote)
      .groupBy(vote.reviewId)
      .as('vote_query');

    const field = input.type === 'course' ? review.courseId : review?.professorId;
    const featuredReview = (
      await db
        .select()
        .from(review)
        .where(eq(field, input.id))
        .leftJoin(voteSubQuery, eq(voteSubQuery.reviewId, review.id))
        .orderBy(desc(review.content), desc(voteSubQuery.score), desc(review.verified)) // featured review criteria
        .limit(1)
    )[0];

    return {
      ...featuredReview.review,
      createdAt: featuredReview.review.createdAt.toISOString(),
      updatedAt: featuredReview.review.updatedAt?.toISOString(),
      score: featuredReview.vote_query?.score ?? 0,
    } as FeaturedReviewData;
  }),

  /**
   * Get avg ratings for a course's professors or a professor's courses
   */
  scores: publicProcedure
    .input(z.object({ type: z.enum(['course', 'professor']), id: z.string() }))
    .query(async ({ input }) => {
      const field = input.type === 'course' ? review.courseId : review.professorId;
      const otherField = input.type === 'course' ? review.professorId : review.courseId;

      const results = await db
        .select({ name: otherField, score: sql`COALESCE(SUM(${vote.vote}), 0)`.mapWith(Number) })
        .from(review)
        .where(eq(field, input.id))
        .leftJoin(vote, eq(vote.reviewId, review.id))
        .groupBy(otherField);

      return results;
    }),
});

export default reviewsRouter;
