import { router, userProcedure } from '../helpers/trpc';
import { RoadmapDiffs, roadmapDiffs, SavedPlannerData, SavedRoadmap } from '@peterportal/types';
import { db, TransactionType } from '../db';
import { planner, user } from '../db/schema';
import { and, count, eq, inArray } from 'drizzle-orm';
import {
  createPlanners,
  createQuarters,
  createYears,
  deletePlannerQuarters,
  deletePlanners,
  deletePlannerYears,
  queryGetPlanners,
  setQuarterCourses,
  updatePlanners,
  updateYears,
} from '../helpers/roadmap';
import { TRPCError } from '@trpc/server';

async function validatePlannerIds(input: RoadmapDiffs, userId: number) {
  const plannerIds = Object.values(input)
    .flat()
    .flatMap((change) => {
      if (typeof change === 'boolean') return [];
      if ('plannerId' in change)
        return [change.plannerId]; // All non-planner operations
      else if ('data' in change)
        return [change.data.id]; // Planner update
      else return [change.id]; // Planner delete
    });
  const uniquePlanIds = [...new Set(plannerIds.filter((id) => id > 0))];

  // Ensure every planner ID belongs to that user (maybe look into RLS in the future)
  const foundPlanIdCount = await db
    .select({ count: count() })
    .from(planner)
    .where(and(eq(planner.userId, userId), inArray(planner.id, uniquePlanIds)))
    .then((c) => c[0].count);

  if (foundPlanIdCount !== uniquePlanIds.length) {
    throw new TRPCError({ code: 'BAD_REQUEST' });
  }
}

async function performRoadmapDeletesAndUpdates(tx: TransactionType, input: RoadmapDiffs, userId: number) {
  if (input.overwrite) return await tx.delete(planner).where(eq(planner.userId, userId));

  const plannerDeletes = deletePlanners(tx, input.deletedPlanners);
  const yearDeletes = deletePlannerYears(tx, input.deletedYears);
  const quarterDeletes = deletePlannerQuarters(tx, input.deletedQuarters);
  await Promise.all([plannerDeletes, yearDeletes, quarterDeletes]);

  const courseUpdates = setQuarterCourses(tx, input.updatedQuarters);
  const yearUpdates = updateYears(tx, input.updatedYears);
  const plannerUpdates = updatePlanners(tx, input.updatedPlanners);
  await Promise.all([courseUpdates, yearUpdates, plannerUpdates]);
}

async function applyRoadmapChanges(input: RoadmapDiffs, userId: number) {
  return await db.transaction(async (tx) => {
    performRoadmapDeletesAndUpdates(tx, input, userId);

    const plannerIdLookup = await createPlanners(tx, input.newPlanners, userId);

    // Update IDs in-place for newly-created planners. This guarantees that all
    // createYears and createQuarters data has the correct input before calling the function
    for (const toInsert of [...input.newYears, ...input.newQuarters]) {
      if (plannerIdLookup[toInsert.plannerId]) toInsert.plannerId = plannerIdLookup[toInsert.plannerId];
    }

    await createYears(tx, input.newYears);
    await createQuarters(tx, input.newQuarters);

    return plannerIdLookup;
  });
}

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
  save: userProcedure.input(roadmapDiffs).mutation(async ({ input, ctx }) => {
    const userId = ctx.session.userId!;

    // When `overwrite` is true, we're never updating/deleting planner data by their planner id;
    // all new info gets written to the current user's account
    if (!input.overwrite) await validatePlannerIds(input, userId);
    const plannerIdLookup = await applyRoadmapChanges(input, userId);

    await db.update(user).set({ lastRoadmapEditAt: new Date() }).where(eq(user.id, userId));

    return plannerIdLookup;
  }),
});

export default roadmapsRouter;
