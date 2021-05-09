var express = require('express');
var router = express.Router();
var fetch = require("node-fetch");
var fs = require('fs');
var path = require('path');
const ObjectID = require('mongodb').ObjectID;
let { COLLECTION_NAMES, getDB, addDocument, getDocuments, updateDocument } = require('../helpers/mongo');

router.get('/', async function (req, res, next) {
  let courseID = req.query.courseID;
  let professorID = req.query.professorID;
  let userID = req.query.userID;

  let query = {
    courseID, professorID, userID
  };

  // remove null params
  for (var param in query) {
    if (query[param] === null || query[param] === undefined) {
      delete query[param];
    }
  }

  let reviews = await getDocuments(COLLECTION_NAMES.REVIEWS, query);

  res.json(reviews);
});

router.post('/', async function (req, res, next) {
  console.log(`Adding Review: ${JSON.stringify(req.body)}`)

  // add review to mongo
  await addDocument(COLLECTION_NAMES.REVIEWS, req.body);

  // echo back body
  res.json(req.body);
});

router.patch('/vote', async function (req, res) {
  let id = req.body["id"];
  let deltaScore = req.body["upvote"] ? 1 : -1;

  console.log(`Voting Review ${id} with delta ${deltaScore}`)

  let status = await updateDocument(COLLECTION_NAMES.REVIEWS,
    { _id: new ObjectID(id) },
    { $inc: { score: deltaScore } });

  res.json(status);
});

module.exports = router;
