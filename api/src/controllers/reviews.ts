/**
 @module ReviewRoute
*/

import express, { Request } from 'express';
import { ObjectId } from 'mongodb';
import { ReviewData, VoteData } from '../types/types';
import { verifyCaptcha } from '../helpers/recaptcha';
import Review from '../models/review';
import Vote from '../models/vote';
const router = express.Router();

/**
 * Get review scores
 */
interface ScoresQuery {
  type: 'course' | 'professor';
  id: string;
}
router.get('/scores', async function (req: Request<never, unknown, never, ScoresQuery>, res) {
  // match filters all reviews with given field
  // group aggregates by field
  let matchField = '';
  let groupField = '';
  if (req.query.type == 'professor') {
    matchField = 'professorID';
    groupField = '$courseID';
  } else if (req.query.type == 'course') {
    matchField = 'courseID';
    groupField = '$professorID';
  }

  // execute aggregation on the reviews collection

  const aggreg = await Review.aggregate([
    { $match: { [matchField]: req.query.id } },
    { $group: { _id: groupField, score: { $avg: '$rating' } } },
  ]);

  // returns the results in an array
  const array = aggreg as ReviewData[];
  // rename _id to name
  const results = array.map((v) => {
    return { name: v._id, score: v.score };
  });
  res.json(results);
});

/**
 * Get featured review
 */
interface FeaturedQuery {
  type: 'course' | 'professor';
  id: string;
}
router.get('/featured', async function (req: Request<never, unknown, never, FeaturedQuery>, res) {
  // search by professor or course field
  let field = '';
  if (req.query.type == 'course') {
    field = 'courseID';
  } else if (req.query.type == 'professor') {
    field = 'professorID';
  }

  // find first review with the highest score
  const reviewsCollection = await Review.find({ [field]: req.query.id })
    .sort({ score: -1 })
    .limit(1);
  if (reviewsCollection) {
    res.json(reviewsCollection);
  } else {
    res.json([]);
  }
});

interface ReviewFilter {
  courseID: string;
  professorID: string;
  userID: string;
  _id?: ObjectId;
  verified?: boolean;
}
/**
 * Query reviews
 */
router.get('/', async function (req, res) {
  const courseID = req.query.courseID as string;
  const professorID = req.query.professorID as string;
  const userID = req.query.userID as string;
  const reviewID = req.query.reviewID as string;
  const verified = req.query.verified as string;

  const query: ReviewFilter = {
    courseID,
    professorID,
    userID,
    _id: reviewID === undefined ? undefined : new ObjectId(reviewID),
    verified: verified === undefined ? undefined : verified === 'true' ? true : false,
  };

  // remove null params
  for (const param in query) {
    if (query[param as keyof ReviewFilter] === null || query[param as keyof ReviewFilter] === undefined) {
      delete query[param as keyof ReviewFilter];
    }
  }

  const reviews = await Review.find(query);
  if (reviews) {
    res.json(reviews);
  } else {
    res.json([]);
  }
});

/**
 * Add a review
 */
router.post('/', async function (req, res) {
  if (req.session.passport) {
    //^ this should be a middleware check smh

    // check if user is trusted
    const verifiedCount = await Review.find({
      userID: req.session.passport.user.id,
      verified: true,
    })
      .countDocuments()
      .exec();

    // Set on server so the client can't automatically verify their own review.
    req.body.verified = verifiedCount >= 3; // auto-verify if use has posted 3+ reviews

    // Verify the captcha
    const verifyResponse = await verifyCaptcha(req.body);
    if (!verifyResponse?.success)
      return res.status(400).json({ error: 'ReCAPTCHA token is invalid', data: verifyResponse });
    delete req.body.captchaToken; // so it doesn't get stored in DB

    //check if review already exists for same professor, course, and user
    const query: ReviewFilter = {
      courseID: req.body.courseID,
      professorID: req.body.professorID,
      userID: req.session.passport.user.id,
    };

    const reviews = await Review.find(query);
    if (reviews?.length > 0)
      return res.status(400).json({ error: 'Review already exists for this professor and course!' });
    // add review to mongo
    req.body.userDisplay =
      req.body.userDisplay === 'Anonymous Peter' ? 'Anonymous Peter' : req.session.passport.user.name;
    req.body.userID = req.session.passport.user.id;
    await new Review(req.body).save();

    // echo back body
    res.json(req.body);
  } else {
    res.json({ error: 'Must be logged in to add a review!' });
  }
});

/**
 * Delete a review
 */
router.delete('/', async (req, res) => {
  const checkUser = async () => {
    const review = (await Review.find({ _id: req.body.id })) as ReviewData[];
    return review.length > 0 && review[0].userID === req.session.passport?.user.id;
  };

  if (req.session.passport?.admin || (await checkUser())) {
    console.log(`Deleting review ${req.body.id}`);
    await Review.deleteOne({ _id: req.body.id });
    await Vote.deleteMany({ reviewID: req.body.id });

    res.status(200).send();
  } else {
    res.json({ error: 'Must be an admin or review author to delete reviews!' });
  }
});

/**
 * Upvote or downvote a review
 */
router.patch('/vote', async function (req, res) {
  if (req.session?.passport != null) {
    //get id and delta score from initial vote
    const id = req.body['id'];
    let deltaScore = req.body['upvote'] ? 1 : -1;
    //query to search for a vote matching the same review and user
    const currentVotes = {
      userID: req.session.passport.user.id,
      reviewID: id,
    };
    //either length 1 or 0 array(ideally) 0 if no existing vote, 1 if existing vote
    const existingVote = (await Vote.find(currentVotes)) as VoteData[];
    //check if there is an existing vote and it has the same vote as the previous vote
    if (existingVote.length != 0 && deltaScore == existingVote[0].score) {
      //remove the vote
      res.json({ deltaScore: -1 * deltaScore });

      //delete the existing vote from the votes collection
      await Vote.deleteMany(currentVotes);
      //update the votes document with a lowered score
      await Review.updateOne({ _id: id }, { $inc: { score: -1 * deltaScore } });
    } else if (existingVote.length != 0 && deltaScore != existingVote[0].score) {
      //there is an existing vote but the vote was different
      deltaScore *= 2;
      //*2 to reverse the old vote and implement the new one
      await Review.updateOne({ _id: id }, { $inc: { score: deltaScore } });
      //override old vote with new data
      await Vote.updateOne({ _id: existingVote[0]._id }, { $set: { score: deltaScore / 2 } });

      res.json({ deltaScore: deltaScore });
    } else {
      //no old vote, just add in new vote data
      console.log(`Voting Review ${id} with delta ${deltaScore}`);
      await Review.updateOne({ _id: id }, { $inc: { score: deltaScore } });
      //sends in vote
      await new Vote({ userID: req.session.passport.user.id, reviewID: id, score: deltaScore }).save();
      res.json({ deltaScore: deltaScore });
    }
  }
});

/**
 * Get whether or not the color of a button should be colored
 */
router.patch('/getVoteColor', async function (req, res) {
  //make sure user is logged in
  if (req.session?.passport != null) {
    //query of the user's email and the review id
    const query = {
      userID: req.session.passport.user.email,
      reviewID: req.body['id'],
    };
    //get any existing vote in the db
    const existingVote = (await Vote.find(query)) as VoteData[];
    //result an array of either length 1 or empty
    if (existingVote.length == 0) {
      //if empty, both should be uncolored
      res.json([false, false]);
    } else {
      //if not empty, there is a vote, so color it accordingly
      if (existingVote[0].score == 1) {
        res.json([true, false]);
      } else {
        res.json([false, true]);
      }
    }
  }
});

/**
 * Get multiple review colors
 */
router.patch('/getVoteColors', async function (req, res) {
  if (req.session?.passport != null) {
    //query of the user's email and the review id
    const ids = req.body['ids'];

    const q = {
      userID: req.session.passport.user.id,
      reviewID: { $in: ids },
    };
    const votes = (await Vote.find(q)) as VoteData[];
    const r: Record<string, number> = {};
    for (let i = 0; i < votes.length; i++) {
      r[votes[i].reviewID] = votes[i].score;
    }
    res.json(r);
  } else {
    res.json({});
  }
});
/*
 * Verify a review
 */
router.patch('/verify', async function (req, res) {
  if (req.session.passport?.admin) {
    console.log(`Verifying review ${req.body.id}`);
    const status = await Review.updateOne({ _id: req.body.id }, { verified: true });
    res.json(status);
  } else {
    res.json({ error: 'Must be an admin to verify reviews!' });
  }
});

/**
 * Clear all reviews
 */
router.delete('/clear', async function (req, res) {
  if (process.env.NODE_ENV != 'production') {
    const status = await Review.deleteMany({});
    res.json(status);
  } else {
    res.json({ error: 'Can only clear on development environment' });
  }
});

export default router;
