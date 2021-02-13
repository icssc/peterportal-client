var express = require('express');
var router = express.Router();
var fetch = require("node-fetch");

router.post('/_search', function(req, res, next) {
  r = fetch(process.env.ELASTIC_ENDPOINT_URL + "/professors/_search", 
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
  r = fetch(process.env.PUBLIC_API_URL + "instructors/" + req.params.ucinetid, {
    headers: {
      'x-api-key': process.env.PPAPI_KEY
    }
  });
  
  r.then((response) => response.json())
  .then((data) => res.send(data))
});

module.exports = router;
