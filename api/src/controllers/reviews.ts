/**
 @module ReviewRoute
*/

import express from 'express';
import { ObjectID } from 'mongodb';
import { VoteData } from '../types/types';
import {
  COLLECTION_NAMES,
  getCollection,
  addDocument,
  getDocuments,
  updateDocument,
  deleteDocument,
  deleteDocuments,
} from '../helpers/mongo';
import { verifyCaptcha } from '../helpers/recaptcha';

const router = express.Router();

/**
 * Get review scores
 */
interface ScoresQuery {
  type: 'course' | 'professor';
  id: string;
}
router.get<{}, {}, {}, ScoresQuery>('/scores', async function (req, res) {
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
  const reviewsCollection = await getCollection(COLLECTION_NAMES.REVIEWS);
  if (reviewsCollection) {
    let cursor = reviewsCollection.aggregate([
      { $match: { [matchField]: req.query.id } },
      { $group: { _id: groupField, score: { $avg: '$rating' } } },
    ]);

    // returns the results in an array
    let array = await cursor.toArray();
    // rename _id to name
    let results = array.map((v) => {
      return { name: v._id, score: v.score };
    });
    res.json(results);
  } else {
    res.json([]);
  }
});

/**
 * Get featured review
 */
interface FeaturedQuery {
  type: 'course' | 'professor';
  id: string;
}
router.get<{}, {}, {}, FeaturedQuery>('/featured', async function (req, res) {
  // search by professor or course field
  let field = '';
  if (req.query.type == 'course') {
    field = 'courseID';
  } else if (req.query.type == 'professor') {
    field = 'professorID';
  }

  // find first review with the highest score
  let reviewsCollection = await getCollection(COLLECTION_NAMES.REVIEWS);
  if (reviewsCollection) {
    let cursor = reviewsCollection
      .find({ [field]: req.query.id })
      .sort({ score: -1 })
      .limit(1);
    let results = await cursor.toArray();
    res.json(results);
  } else {
    res.json([]);
  }
});

interface ReviewFilter {
  courseID: string;
  professorID: string;
  userID: string;
  _id?: ObjectID;
  verified?: boolean;
}
/**
 * Query reviews
 */
router.get('/', async function (req, res, next) {
  let courseID = req.query.courseID as string;
  let professorID = req.query.professorID as string;
  let userID = req.query.userID as string;
  let reviewID = req.query.reviewID as string;
  let verified = req.query.verified as string;

  let query: ReviewFilter = {
    courseID,
    professorID,
    userID,
    _id: reviewID === undefined ? undefined : new ObjectID(reviewID),
    verified: verified === undefined ? undefined : verified === 'true' ? true : false,
  };

  // remove null params
  for (const param in query) {
    if (query[param as keyof ReviewFilter] === null || query[param as keyof ReviewFilter] === undefined) {
      delete query[param as keyof ReviewFilter];
    }
  }

  let reviews = await getDocuments(COLLECTION_NAMES.REVIEWS, query);
  if (reviews) {
    res.json(reviews);
  } else {
    res.json([]);
  }
});

/**
 * Add a review
 */
router.post('/', async function (req, res, next) {
  if (req.session.passport) {
    console.log(req.body);
    //^ this should be a middleware check smh
    console.log(`Adding Review: ${JSON.stringify(req.body)}`);

    // check if user is trusted
    const reviewsCollection = await getCollection(COLLECTION_NAMES.REVIEWS);
    const verifiedCount = await reviewsCollection
      .find({
        userID: req.session.passport.user.id,
        verified: true,
      })
      .count();

    // Set on server so the client can't automatically verify their own review.
    req.body.verified = verifiedCount >= 3; // auto-verify if use has posted 3+ reviews

    // Verify the captcha
    const verifyResponse = await verifyCaptcha(req.body);
    if (!verifyResponse?.success)
      return res.status(400).json({ error: 'ReCAPTCHA token is invalid', data: verifyResponse });
    delete req.body.captchaToken; // so it doesn't get stored in DB

    //check if review already exists for same professor, course, and user
    let query: ReviewFilter = {
      courseID: req.body.courseID,
      professorID: req.body.professorID,
      userID: req.session.passport.user.id,
    };

    let reviews = await getDocuments(COLLECTION_NAMES.REVIEWS, query);
    if (reviews?.length > 0)
      return res.status(400).json({ error: 'Review already exists for this professor and course!' });
    // add review to mongo
    await addDocument(COLLECTION_NAMES.REVIEWS, req.body);

    // echo back body
    res.json(req.body);
  } else {
    res.json({ error: 'Must be logged in to add a review!' });
  }
});

/**
 * Delete a review
 */
router.delete('/', async (req, res, next) => {
  const checkUser = async () => {
    let review = await getDocuments(COLLECTION_NAMES.REVIEWS, {
      _id: new ObjectID(req.body.id),
    });

    return review.length > 0 && review[0].userID === req.session.passport?.user.id;
  };

  if (req.session.passport?.admin || (await checkUser())) {
    console.log(`Deleting review ${req.body.id}`);

    let status = await deleteDocument(COLLECTION_NAMES.REVIEWS, {
      _id: new ObjectID(req.body.id),
    });

    let deleteVotesStatus = await deleteDocuments(COLLECTION_NAMES.VOTES, {
      reviewID: req.body.id,
    });

    res.json(status);
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
    let id = req.body['id'];
    let deltaScore = req.body['upvote'] ? 1 : -1;
    //query to search for a vote matching the same review and user
    let currentVotes = {
      userID: req.session.passport.user.id,
      reviewID: id,
    };
    //either length 1 or 0 array(ideally) 0 if no existing vote, 1 if existing vote
    let existingVote = (await getDocuments(COLLECTION_NAMES.VOTES, currentVotes)) as VoteData[];
    //check if there is an existing vote and it has the same vote as the previous vote
    if (existingVote.length != 0 && deltaScore == existingVote[0].score) {
      //remove the vote
      res.json({ deltaScore: -1 * deltaScore });

      //delete the existing vote from the votes collection
      await deleteDocument(COLLECTION_NAMES.VOTES, currentVotes);
      //update the votes document with a lowered score
      await updateDocument(COLLECTION_NAMES.REVIEWS, { _id: new ObjectID(id) }, { $inc: { score: -1 * deltaScore } });
    } else if (existingVote.length != 0 && deltaScore != existingVote[0].score) {
      //there is an existing vote but the vote was different
      deltaScore *= 2;
      //*2 to reverse the old vote and implement the new one
      await updateDocument(COLLECTION_NAMES.REVIEWS, { _id: new ObjectID(id) }, { $inc: { score: deltaScore } });
      //override old vote with new data
      await updateDocument(COLLECTION_NAMES.VOTES, { _id: existingVote[0]._id }, { $set: { score: deltaScore / 2 } });

      res.json({ deltaScore: deltaScore });
    } else {
      //no old vote, just add in new vote data
      console.log(`Voting Review ${id} with delta ${deltaScore}`);

      await updateDocument(COLLECTION_NAMES.REVIEWS, { _id: new ObjectID(id) }, { $inc: { score: deltaScore } });
      //sends in vote
      await addDocument(COLLECTION_NAMES.VOTES, {
        userID: req.session.passport.user.id,
        reviewID: id,
        score: deltaScore,
      });
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
    let query = {
      userID: req.session.passport.user.email,
      reviewID: req.body['id'],
    };
    //get any existing vote in the db
    let existingVote = await getDocuments(COLLECTION_NAMES.VOTES, query);
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
    let ids = req.body['ids'];
    let colors = [];

    let q = {
      userID: req.session.passport.user.id,
      reviewID: { $in: ids },
    };

    let votes = await getDocuments(COLLECTION_NAMES.VOTES, q);
    let r: any = {};
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

    let status = await updateDocument(
      COLLECTION_NAMES.REVIEWS,
      { _id: new ObjectID(req.body.id) },
      { $set: { verified: true } },
    );

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
    let reviewsCollection = await getCollection(COLLECTION_NAMES.REVIEWS);
    let status = await reviewsCollection.deleteMany({});

    res.json(status);
  } else {
    res.json({ error: 'Can only clear on development environment' });
  }
});

export default router;
