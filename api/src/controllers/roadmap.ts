import Roadmap from '../models/roadmap';
import { router, userProcedure } from '../helpers/trpc';
import { z } from 'zod';
import { mongoRoadmap, MongoRoadmap } from '@peterportal/types';

const roadmapsRouter = router({
  /**
   * Get a user's roadmap
   */
  get: userProcedure.input(z.object({ userID: z.string() })).query(async ({ input }) => {
    const roadmap = await Roadmap.findOne<MongoRoadmap>({ userID: input.userID });
    return roadmap;
  }),
  /**
   * Save a user's roadmap
   */
  save: userProcedure.input(mongoRoadmap).mutation(async ({ input }) => {
    const { userID, roadmap, coursebag } = input;
    if (await Roadmap.exists({ userID })) {
      return await Roadmap.replaceOne({ userID }, { roadmap, userID, coursebag });
    } else {
      // add roadmap to mongo
      return await new Roadmap({ roadmap, userID, coursebag }).save();
    }
  }),
});

export default roadmapsRouter;
