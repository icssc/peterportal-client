var express = require('express');
var router = express.Router();
var fetch = require("node-fetch");
const websoc = require("websoc-api");

//Sakshi: cancel all the APIs
router.post('/_search', function(req, res, next) {
  r = fetch(process.env.PETERPORTAL_MAIN_ES + "courses/_search", 
  {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify(req.body)
  })

  r.then((response) => response.json())
  .then((data) => res.send(data))
});

router.get('/api/:courseID', function(req, res, next) {
  console.log("I do come here");
  r = fetch("https://api.peterportal.org/rest/v0/" + "courses/" + req.params.courseID, {
    headers: {
      'x-api-key': "PeterPortal-293094452aaaaefd3c5ebb7dd1ad4874fb3b4d20e79be6422b9e09611f01589a",
      'Content-Type': 'application/json',
      'Accept': 'application/json'
    }
  });
  
  r.then((response) => response.json())
  .then((data) => res.send(data))
});


router.get('/api/grades/:department/:number', function(req, res, next) {
  r = fetch(process.env.PUBLIC_API_URL + "grades/raw?department=" + encodeURIComponent(req.params.department) + "&number=" + req.params.number, {
    headers: {
      'x-api-key': process.env.PPAPI_KEY
    }
  });
  
  r.then((response) => response.json())
  .then((data) => res.send(data))
});

router.get('/api/schedule/:term/:department/:number', async function(req, res, next) {

  r = await websoc.callWebSocAPI({
    term: req.params.term,
    department: req.params.department,
    courseNumber: req.params.number
  })
  
  res.send(r);
  
  // r.then((response) => response.json())
  // .then((data) => res.send(data))
});

module.exports = router;
