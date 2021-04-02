
var express = require('express');
var router = express.Router();
var fetch = require("node-fetch");
const websoc = require("websoc-api");

router.get('/api/currentWeek', function (req, res, next) {
  r = fetch(process.env.PUBLIC_API_URL + "schedule/currentWeek", {
    headers: {
      'x-api-key': process.env.PPAPI_KEY
    }
  });

  r.then((response) => response.json())
    .then((data) => res.send(data))
});

//Specify our search parameters
router.get('/api/:term/:department/:number', async function (req, res) {
  const result = await websoc.callWebSocAPI({
    term: req.params.term,
    department: req.params.department,
    courseNumber: req.params.number
  });
  res.send(result.schools[0].departments[0].courses[0].sections);
});

module.exports = router;
