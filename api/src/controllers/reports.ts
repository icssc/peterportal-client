/**
 @module ReportsRoute
*/

import express from 'express';
import { ObjectID } from 'mongodb';
import { COLLECTION_NAMES, getCollection, addDocument, getDocuments, deleteDocument } from '../helpers/mongo';

var router = express.Router();

/**
 * Get all reports
 */
router.get('/', async (req, res, next) => {
    console.log('Getting all reports');
    
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
    console.log(`Deleting report ${req.body.id}`);
    
    let status = await deleteDocument(COLLECTION_NAMES.REPORTS, {
        _id: new ObjectID(req.body.id)
    });

    res.json(status);
})

export default router;