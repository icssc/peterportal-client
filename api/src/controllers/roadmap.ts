import express, { Request } from 'express';
import { COLLECTION_NAMES, containsID, addDocument, getDocuments, replaceDocument } from '../helpers/mongo';

const router = express.Router();

/**
 * Get a roadmap
 */
router.get('/get', async function (req: Request<never, unknown, never, { id: string }>, res) {
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
router.post('/', async function (req: Request<never, unknown, { _id: string }, never>, res) {
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
