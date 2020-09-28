var express = require('express');
var {executeQuery, escape} = require('../config/database.js')
var router = express.Router();

/* GET users listing. */
router.get('/', function(req, res, next) {
  res.send('respond with a resource reviews');
});

router.post("/post", function(req, res, next){
  console.log("Hi Poster!", JSON.stringify(req.body));
})

router.get('/professor', function(req, res, next)  {
  let sql = `SELECT * FROM reviews AS r WHERE r.prof_id = ${escape(req.query.profID)} AND r.pub_status != 'removed' ORDER BY r.submitted_at DESC`
  executeQuery(sql, function(results) {
    res.json(results);
  });
})

router.get('/course', function(req, res, next)  {
  let sql = `SELECT * FROM reviews AS r WHERE r.course_id = ${escape(req.query.courseID)} AND r.pub_status != 'removed' ORDER BY r.submitted_at DESC`

  executeQuery(sql, function(results) {
    res.json(results);
  });
})

router.post('/addReview', function(req, res) {
  const data = {
    text: req.body.text,
    rating: req.body.rating,
    difficulty: req.body.difficulty,
    userID: req.user ? `${req.user.userID}`: "NULL",
    courseID: req.body.courseID,
    profID: req.body.profID,
    date: req.body.date,  //format: "2020-02-10"
    grade: req.body.grade,
    takenIn: req.body.takenIn,
    pubStatus: req.user ? "published": "unverified"
  }

  let sql = `INSERT INTO reviews 
  (body, rating, difficulty, user_id, course_id, prof_id, submitted_at, grade, taken_in, pub_status)
  VALUES( ${escape(data.text)}, ${data.rating}, ${data.difficulty}, ${data.userID}, ${escape(data.courseID)}, ${escape(data.profID)}, ${escape(data.date)}, ${escape(data.grade)}, ${escape(data.takenIn)}, ${escape(data.pubStatus)})`

  executeQuery(sql, function(results) {
    res.json(results);
  });
})

router.put('/upVoteReview', function(req, res) {

  let sql = `SELECT * FROM votes WHERE user_id=${req.user.userID} AND review_id=${req.body.reviewID}`

  executeQuery(sql, function(results) {
    //if there is no vote
    if (results.length == 0) {
      sql = `UPDATE reviews SET up_votes = up_votes + 1 WHERE id = ${req.body.reviewID};
      INSERT INTO votes VALUES(${req.user.userID}, ${req.body.reviewID}, true);`

      executeQuery(sql, function(results) {
        res.json(results);
      });
    } else if (!results[0].up) {
      //trying to upvote but it has been downvoted
      res.send("You already downvoted the review.")
    } else { //if it is a upvote delete it
      sql = `UPDATE reviews SET up_votes = up_votes - 1 WHERE id = ${req.body.reviewID};
      DELETE FROM votes WHERE user_id=${req.user.userID} AND review_id=${req.body.reviewID}`

      executeQuery(sql, function(results) {
        res.json(results);
      });
    }
  });
});

router.put('/downVoteReview', function(req, res) {
  let sql = `SELECT * FROM votes WHERE user_id=${req.user.userID} AND review_id=${req.body.reviewID}`

  executeQuery(sql, function(results) {
    //if there is no vote
    if (results.length == 0) {
      sql = `UPDATE reviews SET down_votes = down_votes + 1 WHERE id = ${req.body.reviewID};
      INSERT INTO votes VALUES(${req.user.userID}, ${req.body.reviewID}, false);`

      executeQuery(sql, function(results) {
        res.json(results);
      });
    } else if (results[0].up) {
      //trying to downvote but it has been upvote
      res.send("You already upvoted the review.")

    } else { //if it is a downvote delete it
      sql = `UPDATE reviews SET down_votes = down_votes - 1 WHERE id = ${req.body.reviewID};
      DELETE FROM votes WHERE user_id=${req.user.userID} AND review_id=${req.body.reviewID};`

      executeQuery(sql, function(results) {
        res.json(results);
      });
    }
  });
});

//flag a review for admin to check
router.post('/flagReview', function(req, res) {  
  let sql = `INSERT INTO flagged 
  (user_id, review_id, reason, comments, reported_at, flag_status)
  VALUES(${req.user ? req.user.userID: "NULL"}, ${req.body.reviewID}, ${escape(req.body.reason)}, ${escape(req.body.comments)}, ${escape(req.body.date)}, 'pending')`

  executeQuery(sql, function(results) {
    res.json(results);
  });
});



module.exports = router;
