import express from 'express';
import fetch from 'node-fetch';
const websoc = require('websoc-api');
var router = express.Router();

/**
 * Elasticsearch proxy for courses index
 */
router.post('/_search', function(req, res, next) {
  let r = fetch(process.env.PETERPORTAL_MAIN_ES + 'courses/_search', 
  {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify(req.body)
  })

  r.then((response) => response.json())
  .then((data) => res.send(data))
  .catch(err => console.log('Error:', err))
});

/**
 * PPAPI proxy for professor data 
 */
router.get('/api/:courseID', function(req, res, next) {
  let r = fetch(process.env.PUBLIC_API_URL + 'courses/' + req.params.courseID, {
    headers: {
      'x-api-key': process.env.PPAPI_KEY,
      'Content-Type': 'application/json',
      'Accept': 'application/json'
    }
  });
  
  r.then((response) => response.json())
  .then((data) => res.send(data))
});

/**
 * PPAPI proxy for grade distribution
 */
router.get('/api/grades/:department/:number', function(req, res, next) {
  let r = fetch(process.env.PUBLIC_API_URL + 'grades/raw?department=' + encodeURIComponent(req.params.department) + '&number=' + req.params.number, {
    headers: {
      'x-api-key': process.env.PPAPI_KEY
    }
  });
  
  r.then((response) => response.json())
  .then((data) => res.send(data))
});

export default router;