const { text } = require('express');
var express = require('express');
var router = express.Router();
var fetch = require("node-fetch");

//sakshi: Cancel all the APIs
router.post('/_search', function(req, res, next) {
  r = fetch(process.env.PETERPORTAL_MAIN_ES + "professors/_search", 
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

router.get('/api/:ucinetid', function(req, res, next) {
  r = fetch("https://api.peterportal.org/rest/v0/" + "instructors/" + req.params.ucinetid, {
    headers: {
      'x-api-key': "PeterPortal-293094452aaaaefd3c5ebb7dd1ad4874fb3b4d20e79be6422b9e09611f01589a"
    }
  });
  
  r.then((response) => response.json())
  .then((data) => res.send(data))
});


module.exports = router;
