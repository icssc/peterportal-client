var express = require('express');
var router = express.Router();
var fetch = require("node-fetch");

router.post('/_search', function(req, res, next) {
  r = fetch("https://admin:esN6YPBYvFQ9fhAk@@search-peterportal-main-es-yduwhcbsc6oys5bps3wtlgbnvm.us-west-2.es.amazonaws.com/courses/_search", 
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
  r = fetch(process.env.PUBLIC_API_URL + "courses/" + req.params.courseID);
  
  r.then((response) => response.json())
  .then((data) => res.send(data))
});


router.get('/api/grades/:department/:number', function(req, res, next) {
  console.log(process.env.PUBLIC_API_URL + "grades?department=" + encodeURIComponent(req.params.department) + "&number=" + req.params.number);
  r = fetch(process.env.PUBLIC_API_URL + "grades?department=" + encodeURIComponent(req.params.department) + "&number=" + req.params.number);
  
  r.then((response) => response.json())
  .then((data) => res.send(data))
});

module.exports = router;
