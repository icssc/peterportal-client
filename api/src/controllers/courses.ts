import express from 'express';
import fetch from 'node-fetch';
const websoc = require("websoc-api");
var router = express.Router();

router.post('/_search', function(req, res, next) {
  let r = fetch(process.env.PETERPORTAL_MAIN_ES + "courses/_search", 
  {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify(req.body)
  })

  r.then((response) => response.json())
  .then((data) => res.send(data))
  .catch(err => console.log("Error:", err))
});

router.get('/api/:courseID', function(req, res, next) {
  let r = fetch(process.env.PUBLIC_API_URL + "courses/" + req.params.courseID, {
    headers: {
      'x-api-key': process.env.PPAPI_KEY,
      'Content-Type': 'application/json',
      'Accept': 'application/json'
    }
  });
  
  r.then((response) => response.json())
  .then((data) => res.send(data))
});


router.get('/api/grades/:department/:number', function(req, res, next) {
  let r = fetch(process.env.PUBLIC_API_URL + "grades/raw?department=" + encodeURIComponent(req.params.department) + "&number=" + req.params.number, {
    headers: {
      'x-api-key': process.env.PPAPI_KEY
    }
  });
  
  r.then((response) => response.json())
  .then((data) => res.send(data))
});

router.get('/api/schedule/:term/:department/:number', async function(req, res, next) {

  let r = await websoc.callWebSocAPI({
    term: req.params.term,
    department: req.params.department,
    courseNumber: req.params.number
  })
  
  res.send(r);
});

export default router;