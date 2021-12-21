/**
 @module ReviewRoute
*/

import express from 'express';
import { ObjectID } from 'mongodb';
import { COLLECTION_NAMES, getCollection, addDocument, getDocuments, updateDocument } from '../helpers/mongo';

var router = express.Router();

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
  }
  else if (req.query.type == 'course') {
    matchField = 'courseID';
    groupField = '$professorID';
  }

  // execute aggregation on the reviews collection
  let reviewsCollection = await getCollection(COLLECTION_NAMES.REVIEWS);
  let cursor = reviewsCollection.aggregate([
    { $match: { [matchField]: req.query.id } },
    { $group: { _id: groupField, score: { $avg: "$rating" } } }
  ])

  // returns the results in an array
  let array = await cursor.toArray();
  // rename _id to name
  let results = array.map(v => {
    return { name: v._id, score: v.score }
  });
  res.json(results);
})

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
  }
  else if (req.query.type == 'professor') {
    field = 'professorID';
  }

  // find first review with the highest score
  let reviewsCollection = await getCollection(COLLECTION_NAMES.REVIEWS);
  let cursor = reviewsCollection.find({ [field]: req.query.id }).sort({ score: -1 }).limit(1);
  let results = await cursor.toArray();
  res.json(results);
})

/**
 * Query reviews
 */
router.get('/', async function (req, res, next) {
  let courseID = req.query.courseID as string;
  let professorID = req.query.professorID as string;
  let userID = req.query.userID as string;

  interface ReviewFilter {
    courseID: string,
    professorID: string,
    userID: string
  }

  let query: ReviewFilter = {
    courseID, professorID, userID
  };

  // remove null params 
  for (var param in query) {
    if (query[param as keyof ReviewFilter] === null || query[param as keyof ReviewFilter] === undefined) {
      delete query[param as keyof ReviewFilter];
    }
  }

  let reviews = await getDocuments(COLLECTION_NAMES.REVIEWS, query);

  res.json(reviews);
});

/**
 * Add a review
 */
router.post('/', async function (req, res, next) {
  console.log(`Adding Review: ${JSON.stringify(req.body)}`)

  // add review to mongo
  await addDocument(COLLECTION_NAMES.REVIEWS, req.body);

  // echo back body
  res.json(req.body);
});

/**
 * Upvote or downvote a review
 */
router.patch('/vote', async function (req, res) {
  let id = req.body['id'];
  let deltaScore = req.body['upvote'] ? 1 : -1;

  console.log(`Voting Review ${id} with delta ${deltaScore}`)

  let status = await updateDocument(COLLECTION_NAMES.REVIEWS,
    { _id: new ObjectID(id) },
    { $inc: { score: deltaScore } });

  res.json(status);
});

/**
 * Clear all reviews
 */
router.delete('/clear', async function (req, res) {
  let reviewsCollection = await getCollection(COLLECTION_NAMES.REVIEWS);
  let status = await reviewsCollection.deleteMany({});

  res.json(status);
})

export default router;