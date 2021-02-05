var express = require('express');
var router = express.Router();
var fetch = require("node-fetch");

const FAUNA_GQL_ENDPOINT = 'https://graphql.fauna.com/graphql';

// REST IMPLEMENTATIONS
router.get('/', function (req, res, next) {
  let courseID = req.query.courseID;
  let professorID = req.query.professorID;
  let userID = req.query.userID;
  let queryType = 'all';
  let query = '';

  // determine what to filter by
  if (courseID) queryType = "courseID";
  else if (professorID) queryType = "professorID";
  else if (userID) queryType = "userID";

  let queryFields = `data{
    _id
    professorID
    courseID
    userID
    reviewContent
    rating
    difficulty
    timestamp
    gradeReceived
    forCredit
    score
  }`;

  if (queryType == 'all') {
    query = `
    {
      allReviews{
        ${queryFields}
      }
    }
    `
  }
  else {
    query = `
    query Course($${queryType}: String!) {
      reviewsBy${queryType.charAt(0).toUpperCase() + queryType.slice(1)}(${queryType}: $${queryType}){
        ${queryFields}
      }
    }
    `;
  }

  variables = req.query;

  // execute query an forward result/error
  queryFauna(query, variables)
    .then(result => res.json(result))
    .catch(error => res.json(error));
});

router.post('/', function (req, res, next) {
  // construct graphql query from request body
  let query = `
  mutation {
      createReview(data: {
        ${Object.keys(req.body).map(field => {
    // transform js object to query format
    let value = req.body[field];

    // wrap string in quotes
    if (typeof value == "string") {
      value = '"' + value + '"';
    }

    return field + ':' + value;
  }).join("\n")}
      }){
        ${Object.keys(req.body).join("\n")}
      }
    }
  `;

  // execute query an forward result/error
  queryFauna(query)
    .then(result => res.json(result))
    .catch(error => res.json(error));
});

router.patch('/vote', async function (req, res) {
  let id = req.body["id"];
  let deltaScore = req.body["upvote"] ? 1 : -1;

  // retreive the review's current score
  let query = `
  query ($id: ID!){
    findReviewByID(id: $id) {
      professorID
      courseID
      userID
      reviewContent
      rating
      difficulty
      timestamp
      gradeReceived
      forCredit
      score
    }
  }
  `
  let review = await queryFauna(query, req.body);
  review = review["data"]["findReviewByID"];

  // if review does not exist
  if (!review) {
    res.json({ error: "Review Does Not Exist" });
  }
  else {
    review["score"] += deltaScore;
    // mutate the review's score
    query = `
    mutation ($data: ReviewInput!, $id: ID!) {
      updateReview (data: $data
        id: $id){
        score
      }
    }
    `
    let updatedReview = await queryFauna(query, { id: id, data: review });
    res.json(updatedReview);
  }
});

function queryFauna(query, variables) {
  return new Promise((resolve, reject) => {
    let headers = { 'Content-Type': 'application/json', 'Authorization': 'Bearer ' + process.env.FAUNA_SECRET };
    let body = {
      query: query,
      variables: variables
    }

    fetch(FAUNA_GQL_ENDPOINT, {
      method: 'POST',
      headers: headers,
      body: JSON.stringify(body)
    })
      .then(response => response.json())
      .then(json => resolve(json))
      .catch(err => reject(err));
  })
}

module.exports = router;
