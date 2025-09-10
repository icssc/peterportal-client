import { router, userProcedure } from '../helpers/trpc';
import { SavedPlannerData, savedRoadmap, SavedRoadmap } from '@peterportal/types';
import { db } from '../db';
import { planner, user } from '../db/schema';
import { and, eq, inArray, not } from 'drizzle-orm';
import { queryGetPlanners } from '../helpers/roadmap';

const roadmapsRouter = router({
  /**
   * Get a user's roadmap
   */
  get: userProcedure.query(async ({ ctx }) => {
    const [planners, timestamp] = await Promise.all([
      queryGetPlanners(eq(planner.userId, ctx.session.userId!)),
      db.select({ timestamp: user.lastRoadmapEditAt }).from(user).where(eq(user.id, ctx.session.userId!)),
    ]);
    if (planners.length === 0) {
      return undefined;
    }
    const roadmap: SavedRoadmap = {
      planners: planners as SavedPlannerData[],
      timestamp: timestamp[0].timestamp?.toISOString(),
    };
    return roadmap;
  }),
  /**
   * Save a user's roadmap
   */
  save: userProcedure.input(savedRoadmap).mutation(async ({ input, ctx }) => {
    const { planners, timestamp } = input;
    const userId = ctx.session.userId!;

    const plannerUpdates = planners
      .filter((planner) => planner.id >= 0)
      .map((plannerData) =>
        db
          .update(planner)
          .set({ name: plannerData.name, years: plannerData.content })
          .where(and(eq(planner.userId, userId), eq(planner.id, plannerData.id!))),
      );

    const newPlannersToAdd = planners
      .filter((planner) => planner.id < 0)
      .map((planner) => ({ userId, name: planner.name, years: planner.content }));

    // Delete any existing planners that are not in the planners array (user removed them), then insert any new planners (to avoid race when deleting new ones)
    const plannerInsertionsAndDeletions = db
      .delete(planner)
      .where(
        and(
          eq(planner.userId, userId),
          not(inArray(planner.id, planners.map((p) => p.id).filter((id) => id !== undefined) as number[])),
        ),
      )
      .then(() => {
        if (newPlannersToAdd.length > 0) {
          return db.insert(planner).values(newPlannersToAdd);
        }
      });

    const updateLastEditTimestamp = db
      .update(user)
      .set({ lastRoadmapEditAt: new Date(timestamp!) })
      .where(eq(user.id, userId));

    await Promise.all([...plannerUpdates, plannerInsertionsAndDeletions, updateLastEditTimestamp]);
  }),
});

export default roadmapsRouter;
