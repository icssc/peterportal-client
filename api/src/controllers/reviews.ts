/**
 @module ReviewRoute
*/

import express from "express";
import { ObjectID } from "mongodb";
import { VoteData } from "../types/types";
import {
  COLLECTION_NAMES,
  getCollection,
  addDocument,
  getDocuments,
  updateDocument,
} from "../helpers/mongo";

var router = express.Router();

/**
 * Get review scores
 */
interface ScoresQuery {
  type: "course" | "professor";
  id: string;
}
router.get<{}, {}, {}, ScoresQuery>("/scores", async function (req, res) {
  // match filters all reviews with given field
  // group aggregates by field
  let matchField = "";
  let groupField = "";
  if (req.query.type == "professor") {
    matchField = "professorID";
    groupField = "$courseID";
  } else if (req.query.type == "course") {
    matchField = "courseID";
    groupField = "$professorID";
  }

  // execute aggregation on the reviews collection
  let reviewsCollection = await getCollection(COLLECTION_NAMES.REVIEWS);
  let cursor = reviewsCollection.aggregate([
    { $match: { [matchField]: req.query.id } },
    { $group: { _id: groupField, score: { $avg: "$rating" } } },
  ]);

  // returns the results in an array
  let array = await cursor.toArray();
  // rename _id to name
  let results = array.map((v) => {
    return { name: v._id, score: v.score };
  });
  res.json(results);
});

/**
 * Get featured review
 */
interface FeaturedQuery {
  type: "course" | "professor";
  id: string;
}
router.get<{}, {}, {}, FeaturedQuery>("/featured", async function (req, res) {
  // search by professor or course field
  let field = "";
  if (req.query.type == "course") {
    field = "courseID";
  } else if (req.query.type == "professor") {
    field = "professorID";
  }

  // find first review with the highest score
  let reviewsCollection = await getCollection(COLLECTION_NAMES.REVIEWS);
  let cursor = reviewsCollection
    .find({ [field]: req.query.id })
    .sort({ score: -1 })
    .limit(1);
  let results = await cursor.toArray();
  res.json(results);
});

/**
 * Query reviews
 */
router.get("/", async function (req, res, next) {
  let courseID = req.query.courseID as string;
  let professorID = req.query.professorID as string;
  let userID = req.query.userID as string;
  let reviewID = req.query.reviewID as string;

  interface ReviewFilter {
    courseID: string,
    professorID: string,
    userID: string
    _id: ObjectID | undefined
  }

  let query: ReviewFilter = {
    courseID, professorID, userID, _id: (reviewID === undefined ? undefined : new ObjectID(reviewID))
  };

  // remove null params
  for (var param in query) {
    if (
      query[param as keyof ReviewFilter] === null ||
      query[param as keyof ReviewFilter] === undefined
    ) {
      delete query[param as keyof ReviewFilter];
    }
  }

  let reviews = await getDocuments(COLLECTION_NAMES.REVIEWS, query);

  res.json(reviews);
});

/**
 * Add a review
 */
router.post("/", async function (req, res, next) {
  console.log(`Adding Review: ${JSON.stringify(req.body)}`);

  // add review to mongo
  await addDocument(COLLECTION_NAMES.REVIEWS, req.body);

  // echo back body
  res.json(req.body);
});

/**
 * Delete a review
 */
router.delete('/', async (req, res, next) => {
  console.log(`Deleting review ${req.body.id}`);

  let status = await deleteDocument(COLLECTION_NAMES.REVIEWS, {
    _id: new ObjectID(req.body.id)
  });

  res.json(status);
})

/**
 * Upvote or downvote a review
 */
router.patch("/vote", async function (req, res) {
  if (req.session.passport != null) {
    //get id and delta score from initial vote
    let id = req.body["id"];
    let deltaScore = req.body["upvote"] ? 1 : -1;
    //query to search for a vote matching the same review and user
    let currentVotes = {
      userID: req.session.passport.user.email,
      reviewID: id,
    };
    //either length 1 or 0 array(ideally) 0 if no existing vote, 1 if existing vote
    let existingVote = await getDocuments(COLLECTION_NAMES.VOTES, currentVotes) as VoteData[];
    //check if there is an existing vote and it has the same vote as the previous vote
    if (existingVote.length != 0 && deltaScore == existingVote[0].score) {
      res.json({ deltaScore: 0 });
      //do nothing if the vote is the same
    } else if (existingVote.length != 0 && deltaScore != existingVote[0].score) {
      //there is an existing vote but the vote was different
      deltaScore *= 2;
      //*2 to reverse the old vote and implement the new one
      await updateDocument(
        COLLECTION_NAMES.REVIEWS,
        { _id: new ObjectID(id) },
        { $inc: { score: deltaScore } }
      );
      //override old vote with new data
      await updateDocument(COLLECTION_NAMES.VOTES,
        { _id: existingVote[0]._id },
        { $set: { score: deltaScore / 2 } }
      );

      res.json({ deltaScore: deltaScore });
    } else {
      //no old vote, just add in new vote data
      console.log(`Voting Review ${id} with delta ${deltaScore}`);

      await updateDocument(
        COLLECTION_NAMES.REVIEWS,
        { _id: new ObjectID(id) },
        { $inc: { score: deltaScore } }
      );
      //sends in vote
      await addDocument(COLLECTION_NAMES.VOTES, {
        userID: req.session.passport.user.email,
        reviewID: id,
        score: deltaScore,
      });

      res.json({ deltaScore: deltaScore });
    }
  }
});

/**
 * Clear all reviews
 */
router.delete("/clear", async function (req, res) {
  let reviewsCollection = await getCollection(COLLECTION_NAMES.REVIEWS);
  let status = await reviewsCollection.deleteMany({});

  res.json(status);
});

export default router;
