/**
 @module ReportsRoute
*/

import express from 'express';
import { ObjectID } from 'mongodb';
import {
  COLLECTION_NAMES,
  getCollection,
  addDocument,
  getDocuments,
  deleteDocument,
  deleteDocuments,
} from '../helpers/mongo';
import { GenericObject } from '../types/types';

var router = express.Router();

/**
 * Get all reports
 */
router.get('/', async (req, res, next) => {
  let reports = await getDocuments(COLLECTION_NAMES.REPORTS, {}); // get all reports in collection

  res.json(reports);
});

/**
 * Add a report
 */
router.post('/', async (req, res, next) => {
  console.log(`Adding Report: ${JSON.stringify(req.body)}`);

  await addDocument(COLLECTION_NAMES.REPORTS, req.body);

  res.json(req.body);
});

/**
 * Delete a report
 */
router.delete('/', async (req, res, next) => {
  let status;
  if (req.body.id) {
    console.log(`Deleting report ${req.body.id}`);
    status = await deleteDocument(COLLECTION_NAMES.REPORTS, {
      _id: new ObjectID(req.body.id),
    });
  } else {
    console.log(`Deleting reports with reviewID ${req.body.reviewID}`);
    let query: GenericObject = {};
    if (req.body.reviewID) query['reviewID'] = req.body.reviewID;

    if (Object.keys(query).length === 0) return; // avoid deleting all documents if no filters are specified

    status = await deleteDocuments(COLLECTION_NAMES.REPORTS, query);
  }

  res.json(status);
});

export default router;
