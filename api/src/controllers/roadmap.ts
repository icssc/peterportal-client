import { router, userProcedure } from '../helpers/trpc';
import { SavedPlannerData, savedRoadmap, SavedRoadmap, TransferData } from '@peterportal/types';
import { db } from '../db';
import { planner, transferredCourse, user } from '../db/schema';
import { and, asc, eq, inArray, not } from 'drizzle-orm';

const roadmapsRouter = router({
  /**
   * Get a user's roadmap
   */
  get: userProcedure.query(async ({ ctx }) => {
    const [planners, transfers, timestamp] = await Promise.all([
      db
        .select({ id: planner.id, name: planner.name, content: planner.years })
        .from(planner)
        .where(eq(planner.userId, ctx.session.userId!))
        .orderBy(asc(planner.id)),
      db
        .select({ name: transferredCourse.courseName, units: transferredCourse.units })
        .from(transferredCourse)
        .where(eq(transferredCourse.userId, ctx.session.userId!)),
      db.select({ timestamp: user.lastRoadmapEditAt }).from(user).where(eq(user.id, ctx.session.userId!)),
    ]);
    const roadmap: SavedRoadmap = {
      planners: planners as SavedPlannerData[],
      transfers: transfers as TransferData[],
      timestamp: timestamp[0].timestamp?.toISOString(),
    };
    return roadmap;
  }),
  /**
   * Save a user's roadmap
   */
  save: userProcedure.input(savedRoadmap).mutation(async ({ input, ctx }) => {
    const { planners, transfers, timestamp } = input;
    const userId = ctx.session.userId!;

    const plannerUpdates = planners
      .filter((planner) => planner.id !== undefined)
      .map((planner_) =>
        db.update(planner).set({ name: planner_.name, years: planner_.content }).where(eq(planner.id, planner_.id!)),
      );

    const newPlannersToAdd = planners
      .filter((planner) => planner.id === undefined)
      .map((planner) => ({ userId, name: planner.name, years: planner.content }));

    // Delete any existing planners that are not in the planners array (user removed them)
    await db
      .delete(planner)
      .where(
        and(
          eq(planner.userId, userId),
          not(inArray(planner.id, planners.map((p) => p.id).filter((id) => id !== undefined) as number[])),
        ),
      );

    await Promise.all([
      ...plannerUpdates,
      ...(newPlannersToAdd.length > 0 ? [db.insert(planner).values(newPlannersToAdd)] : []),
      db
        .delete(transferredCourse)
        .where(eq(transferredCourse.userId, userId))
        .then(() => {
          return db
            .insert(transferredCourse)
            .values(transfers.map((transfer) => ({ userId, courseName: transfer.name, units: transfer.units })));
        }),
      db
        .update(user)
        .set({ lastRoadmapEditAt: new Date(timestamp!) })
        .where(eq(user.id, userId)),
    ]);
  }),
});

export default roadmapsRouter;
