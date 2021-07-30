import express from 'express';
import { getWeek } from '../helpers/week';

const websoc = require('websoc-api');
var router = express.Router();

/**
 * Get the current week
 */
router.get('/api/currentWeek', function (req, res, next) {
  getWeek().then(week => res.send(week))
});

/**
 * Proxy for WebSOC
 */
router.get('/api/:term/:department/:number', async function (req, res) {
  const result = await websoc.callWebSocAPI({
    term: req.params.term,
    department: req.params.department,
    courseNumber: req.params.number
  });
  console.log(result)
  try {
    console.log(result.schools[0].departments[0].courses[0].sections);
  }
  catch (error) {
    // No school/department/course
    if (error instanceof TypeError) {
      res.send([]);
    }
  }
  res.send(result.schools[0].departments[0].courses[0].sections);
});

export default router;