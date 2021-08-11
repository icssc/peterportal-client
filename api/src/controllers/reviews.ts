/**
 @module ReviewRoute
*/

import express from 'express';
import { ObjectID } from 'mongodb';
import { COLLECTION_NAMES, getCollection, addDocument, getDocuments, updateDocument } from '../helpers/mongo';

var router = express.Router();

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

router.get('/clear', async function (req, res) {
  let reviewsCollection = await getCollection(COLLECTION_NAMES.REVIEWS);
  let status = await reviewsCollection.deleteMany({});

  res.json(status);
})

export default router;