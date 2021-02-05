var express = require('express');
var router = express.Router();
var fetch = require("node-fetch");

router.get('/api/currentWeek', function(req, res, next) {

  r = fetch(process.env.PUBLIC_API_URL + "schedule/currentWeek", {
    headers: {
      'x-api-key': process.env.PPAPI_KEY
    }
  });
  

  r.then((response) => response.json())
  .then((data) => res.send(data))
});

module.exports = router;
