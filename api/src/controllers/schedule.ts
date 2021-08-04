/**
 @module ScheduleRoute
*/

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
  res.send(result);
});

export default router;