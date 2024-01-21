import express from 'express';
import { ObjectID } from 'mongodb';
import {
  COLLECTION_NAMES,
  containsID,
  getCollection,
  addDocument,
  getDocuments,
  replaceDocument,
} from '../helpers/mongo';

var router = express.Router();

/**
 * Get a roadmap
 */
router.get<{}, {}, {}, { id: string }>('/get', async function (req, res) {
  getDocuments(COLLECTION_NAMES.ROADMAPS, { _id: req.query.id }).then((roadmaps) => {
    if (roadmaps.length > 0) {
      res.json(roadmaps[0]);
    } else {
      res.json({ error: 'No roadmap found!' });
    }
  });
});

/**
 * Add a roadmap
 */
router.post<{}, {}, { _id: string }>('/', async function (req, res, next) {
  if (!req.body._id) {
    res.json({ error: 'Invalid input' });
    return;
  }
  console.log(`Adding Roadmap: ${JSON.stringify(req.body)}`);

  if (await containsID(COLLECTION_NAMES.ROADMAPS, req.body._id)) {
    // overwrite
    await replaceDocument(COLLECTION_NAMES.ROADMAPS, { _id: req.body._id }, req.body);
  } else {
    // add roadmap to mongo
    await addDocument(COLLECTION_NAMES.ROADMAPS, req.body);
  }

  res.json({});
});

export default router;
