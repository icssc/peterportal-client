
var express = require('express');
var router = express.Router();
var fetch = require("node-fetch");
const websoc = require("websoc-api");


//Specify our search parameters




router.get('/api/:term/:department/:number', async function (req, res) {
  console.log("Working from here");
	const result = await websoc.callWebSocAPI({
    term: req.params.term,
    department: req.params.department,
    courseNumber: req.params.number
  });
	res.send(result.schools[0].departments[0].courses[0].sections);
});





module.exports = router;
