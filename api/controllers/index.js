var express = require('express');
var app = express();
var fetch = require("node-fetch");
const websoc = require("websoc-api");


  //Specify our search parameters
  const opts = {
	  term: '2019 Fall',
	  department: 'I&C SCI',
	  courseNumber: '32A'
  }



app.get('/schedule', async function (req, res) {

	const result = await websoc.callWebSocAPI(opts);
	res.send(result.schools[0].departments[0].courses[0].sections);
});

app.listen(3000, function () {
  console.log('Example app listening on port  !');
});

