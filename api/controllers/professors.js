var express = require('express');
var router = express.Router();
var fetch = require("node-fetch");

router.post('/_search', function(req, res, next) {
  r = fetch(process.env.PETERPORTAL_MAIN_ES + "/professors/_search", 
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

module.exports = router;
