/**
 @module ReportsRoute
*/

import express from 'express';
import { GenericObject } from '../types/types';
import Report from '../models/report';
const router = express.Router();

/**
 * Get all reports
 */
router.get('/', async (req, res) => {
  // get all reports in collection
  const reports = await Report.find({});

  res.json(reports);
});

/**
 * Add a report
 */
router.post('/', async (req, res) => {
  console.log(`Adding Report: ${JSON.stringify(req.body)}`);
  const report = new Report(req.body);
  await report.save();

  res.json(req.body);
});

/**
 * Delete a report
 */
router.delete('/', async (req, res) => {
  let status;
  if (req.body.id) {
    console.log(`Deleting report ${req.body.id}`);
    status = await Report.deleteOne({ _id: req.body.id });
  } else {
    console.log(`Deleting reports with reviewID ${req.body.reviewID}`);
    const query: GenericObject = {};
    if (req.body.reviewID) query['reviewID'] = req.body.reviewID;

    if (Object.keys(query).length === 0) return; // avoid deleting all documents if no filters are specified

    status = await Report.deleteMany(query);
  }

  res.json(status);
});

export default router;
