import express, { Request } from 'express';
import Roadmap from '../models/roadmap';
const router = express.Router();

/**
 * Get a roadmap
 */
router.get('/get', async function (req: Request<never, unknown, Record<string, unknown>, { id: string }>, res) {
  const userID = req.query.id;
  Roadmap.findOne({ userID })
    .then((roadmap) => {
      if (roadmap) {
        res.json(roadmap);
      } else {
        res.json({ error: 'No roadmap found!' });
      }
    })
    .catch(() => {
      res.json({ error: 'Cannot get roadmap' });
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

  try {
    if (await Roadmap.exists({ userID: req.body._id })) {
      await Roadmap.replaceOne(
        { userID: req.body._id },
        { roadmap: req.body.roadmap, userID: req.body._id, coursebag: req.body.coursebag },
      );
    } else {
      // add roadmap to mongo
      await new Roadmap({ roadmap: req.body.roadmap, userID: req.body._id, coursebag: req.body.coursebag }).save();
    }
    res.json({});
  } catch {
    res.json({ error: 'Cannot save roadmap' });
  }
});

export default router;
