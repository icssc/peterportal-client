/**
 @module ScheduleRoute
*/

import express from 'express';

const router = express.Router();

const TERM_SEASONS = ['Winter', 'Spring', 'Summer1', 'Summer10wk', 'Summer2', 'Fall'];

router.get('/getTerms', function (req, res) {
  let pastYears: number = parseInt(req.query.years as string);
  if (!pastYears) {
    pastYears = 1;
  }
  const d = new Date();
  const year = d.getFullYear();
  const terms = [];
  for (let y = year - pastYears; y <= year; ++y) {
    for (let i = 0; i < TERM_SEASONS.length; ++i) {
      terms.push(`${y} ${TERM_SEASONS[i]}`);
    }
  }
  res.json(terms);
});

/**
 * Get the current week
 */
router.get('/api/currentWeek', async function (_, res) {
  const apiResp = await fetch(`${process.env.PUBLIC_API_URL}week`);
  const json = await apiResp.json();
  res.send(json.payload);
});

/**
 * Get the current quarter on websoc
 */
router.get('/api/currentQuarter', async function (_, res) {
  const apiResp = await fetch(`${process.env.PUBLIC_API_URL}websoc/terms`);
  const json = await apiResp.json();
  res.send(json.payload[0]);
});

/**
 * Proxy for WebSOC, using PeterPortal API
 */
router.get('/api/:term/:department/:number', async function (req, res) {
  const [year, quarter] = req.params.term.split(' ');
  const result = await callPPAPIWebSoc({
    year,
    quarter,
    department: req.params.department,
    courseNumber: req.params.number,
  });
  res.send(result);
});

/**
 * Proxy for WebSOC, using PeterPortal API
 */
router.get('/api/:term/:professor', async function (req, res) {
  const [year, quarter] = req.params.term.split(' ');
  const result = await callPPAPIWebSoc({
    year,
    quarter,
    instructorName: req.params.professor,
  });
  res.send(result);
});

async function callPPAPIWebSoc(params: Record<string, string>) {
  const url: URL = new URL(process.env.PUBLIC_API_URL + 'websoc?' + new URLSearchParams(params));
  return await fetch(url)
    .then((response) => response.json())
    .then((json) => json.payload);
}

export default router;
