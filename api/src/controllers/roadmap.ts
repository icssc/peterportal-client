import express, { Request } from 'express';
import Roadmap from '../models/roadmap';
const router = express.Router();

/**
 * Get a roadmap
 */
router.get('/get', async function (req: Request<never, unknown, never, { id: string }>, res) {
  Roadmap.findById(req.query.id).then((roadmap) => {
    if (roadmap) {
      res.json(roadmap);
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
  if (await Roadmap.exists({ _id: req.body._id })) {
    await Roadmap.replaceOne({ _id: req.body._id }, req.body);
  } else {
    // add roadmap to mongo
    await new Roadmap(req.body).save();
  }

  res.json({});
});

export default router;
