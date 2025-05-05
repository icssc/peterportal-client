import { router, userProcedure } from '../helpers/trpc';
import { db } from '../db';
import { transferredMisc } from '../db/schema';
import { and, eq } from 'drizzle-orm';
import { z } from 'zod';

/** @todo complete all routes. We will remove comments after all individual PRs are merged to avoid merge conflicts */
const transferCreditsRouter = router({
  /** @todo add user procedure to get transferred courses below this comment. */
  /** @todo add user procedure to get transferred AP Exams below this comment. */
  /** @todo add user procedure to get transferred GE credits below this comment. */
  /** @todo add user procedure to get transferred untransferred credits below this comment. */
  getUncategorizedTransfers: userProcedure.query(async ({ ctx }) => {
    const courses = await db
      .select({ name: transferredMisc.courseName, units: transferredMisc.units })
      .from(transferredMisc)
      .where(eq(transferredMisc.userId, ctx.session.userId!));
    return courses;
  }),

  removeUncategorizedCourse: userProcedure
    .input(z.object({ name: z.string().nullable(), units: z.number().nullable() }))
    .mutation(async ({ ctx, input }) => {
      const conditions = [eq(transferredMisc.userId, ctx.session.userId!)];

      if (input.name != null) {
        conditions.push(eq(transferredMisc.courseName, input.name));
      }

      if (input.units != null) {
        conditions.push(eq(transferredMisc.units, input.units));
      }

      await db.delete(transferredMisc).where(and(...conditions));
    }),
});

export default transferCreditsRouter;
