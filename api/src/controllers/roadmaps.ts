import { router, userProcedure } from '../helpers/trpc';
import { SavedPlannerData, savedRoadmap, SavedRoadmap } from '@peterportal/types';
import { db } from '../db';
import { planner, transferredMisc, user } from '../db/schema';
import { and, asc, eq, inArray, not } from 'drizzle-orm';

const roadmapsRouter = router({
  /**
   * Get a user's roadmaps
   */
  get: userProcedure.query(async ({ ctx }) => {
    const [planners, users] = await Promise.all([
      db
        .select({ id: planner.id, name: planner.name, content: planner.years })
        .from(planner)
        .where(eq(planner.userId, ctx.session.userId!))
        .orderBy(asc(planner.id)),
      db
        .select({ timestamp: user.lastRoadmapEditAt, currentPlanIndex: user.currentPlanIndex })
        .from(user)
        .where(eq(user.id, ctx.session.userId!)),
    ]);

    if (planners.length === 0) {
      return undefined;
    }

    const roadmap: SavedRoadmap = {
      planners: planners as SavedPlannerData[],
      transfers: [],
      timestamp: users[0].timestamp?.toISOString(),
      currentPlanIndex: users[0].currentPlanIndex ?? undefined,
    };
    return roadmap;
  }),
  /**
   * Save a user's roadmap
   */
  save: userProcedure.input(savedRoadmap).mutation(async ({ input, ctx }) => {
    const { planners, transfers, timestamp, currentPlanIndex } = input;
    const userId = ctx.session.userId!;

    const plannerUpdates = planners
      .filter((planner) => planner.id !== undefined)
      .map((plannerData) =>
        db
          .update(planner)
          .set({ name: plannerData.name, years: plannerData.content })
          .where(eq(planner.id, plannerData.id!)),
      );

    const newPlannersToAdd = planners
      .filter((planner) => planner.id === undefined)
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

    const replaceTransferredCourses = db
      .delete(transferredMisc)
      .where(eq(transferredMisc.userId, userId))
      .then(() => {
        if (transfers.length > 0) {
          return db
            .insert(transferredMisc)
            .values(transfers.map((transfer) => ({ userId, courseName: transfer.name, units: transfer.units })));
        }
      });

    const updateUser = db
      .update(user)
      .set({ lastRoadmapEditAt: new Date(timestamp!), currentPlanIndex })
      .where(eq(user.id, userId));

    await Promise.all([...plannerUpdates, plannerInsertionsAndDeletions, replaceTransferredCourses, updateUser]);
  }),
});

export default roadmapsRouter;
