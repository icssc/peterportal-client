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
  if (!req.session.passport) return res.status(401).send('Unathenticated');
  if (!req.session.passport.admin) return res.status(403).send('Unauthorized');

  Report.find({})
    .then((reports) => {
      if (reports) {
        res.json(reports);
      } else {
        res.json({ error: 'No reports found!' });
      }
    })
    .catch(() => {
      res.json({ error: 'Cannot get reports' });
    });
});

/**
 * Add a report
 */
router.post('/', async (req, res) => {
  console.log(`Adding Report: ${JSON.stringify(req.body)}`);
  const report = new Report(req.body);
  report
    .save()
    .then(() => {
      res.json(req.body);
    })
    .catch(() => {
      res.json({ error: 'Cannot add report' });
    });
});

/**
 * Delete a report
 */
router.delete('/', async (req, res) => {
  try {
    let status;
    if (!req.session.passport) return res.status(401).send('Unathenticated');
    if (!req.session.passport.admin) return res.status(403).send('Unauthorized');
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
  } catch {
    res.json({ error: 'Cannot delete report' });
  }
});

export default router;
