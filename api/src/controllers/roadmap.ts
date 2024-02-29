import express, { Request } from 'express';
import Roadmap from '../models/roadmap';
const router = express.Router();

/**
 * Get a roadmap
 */
router.get('/get', async function (req: Request<never, unknown, Record<string, unknown>, { id: string }>, res) {
  Roadmap.findOne({ userID: req.body?._id }).then((roadmap) => {
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
router.post('/', async function (req: Request<never, unknown, Record<string, unknown>, never>, res) {
  if (!req.body._id) {
    res.json({ error: 'Invalid input' });
    return;
  }
  console.log(`Adding Roadmap: ${JSON.stringify(req.body)}`);
  if (await Roadmap.exists({ userID: req.body._id })) {
    await Roadmap.replaceOne({ userID: req.body._id }, { roadmap: req.body.roadmap, userID: req.body._id });
  } else {
    // add roadmap to mongo
    await new Roadmap({ roadmap: req.body.roadmap, userID: req.body._id }).save();
  }

  res.json({});
});

export default router;
