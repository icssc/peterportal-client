/**
 @module ScheduleRoute
*/

import express from 'express';
import { getWeek } from '../helpers/week';
import { getCurrentQuarter } from '../helpers/currentQuarter';
import fetch from 'node-fetch';

var router = express.Router();

const TERM_SEASONS = ['Winter', 'Spring', 'Summer1', 'Summer10wk', 'Summer2', 'Fall']

router.get("/getTerms", function (req, res) {
  let pastYears: number = parseInt(req.query.years as string);
  if (!pastYears) {
    pastYears = 1;
  }
  let d = new Date();
  let year = d.getFullYear();
  let terms = [];
  for (let y = year - pastYears; y <= year; ++y) {
    for (let i = 0; i < TERM_SEASONS.length; ++i) {
      terms.push(`${y} ${TERM_SEASONS[i]}`);
    }
  }
  res.json(terms);
})

/**
 * Get the current week
 */
router.get('/api/currentWeek', function (req, res, next) {
  getWeek().then(week => res.send(week))
});

/**
 * Get the current quarter on websoc
 */
 router.get('/api/currentQuarter', function (req, res, next) {
  getCurrentQuarter().then(currentQuarter => res.send(currentQuarter))
});


/**
 * Proxy for WebSOC, using PeterPortal API
 */
router.get('/api/:term/:department/:number', async function (req, res) {
  const result = await callPPAPIWebSoc({
    term: req.params.term,
    department: req.params.department,
    courseNumber: req.params.number
  });
  res.send(result);
});

/**
 * Proxy for WebSOC, using PeterPortal API
 */
router.get('/api/:term/:professor', async function (req, res) {
  const result = await callPPAPIWebSoc({
    term: req.params.term,
    instructorName: req.params.professor
  });
  res.send(result);
});

async function callPPAPIWebSoc(params: Record<string, string>) {
  const url: URL = new URL(process.env.PUBLIC_API_URL + 'schedule/soc?' + 
    new URLSearchParams(params))
  return await fetch(url).then(response => response.json());
}

export default router;