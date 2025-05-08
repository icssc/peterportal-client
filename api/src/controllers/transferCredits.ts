import { and, eq, isNull } from 'drizzle-orm';
import { db } from '../db';
import { transferredCourse } from '../db/schema';
import { router, userProcedure } from '../helpers/trpc';
import { transferredMisc } from '../db/schema';
import { z } from 'zod';

/** @todo complete all routes. We will remove comments after all individual PRs are merged to avoid merge conflicts */
const transferCreditsRouter = router({
  /** @todo add user procedure to get transferred courses below this comment. */
  getTransferredCourses: userProcedure.query(async ({ ctx }) => {
    const response = await db
      .select({ courseName: transferredCourse.courseName, units: transferredCourse.units })
      .from(transferredCourse)
      .where(eq(transferredCourse.userId, ctx.session.userId!));
    return response;
  }),
  addTransferredCourse: userProcedure
    .input(z.object({ courseName: z.string(), units: z.number() }))
    .mutation(async ({ ctx, input }) => {
      await db
        .insert(transferredCourse)
        .values({ courseName: input.courseName, units: input.units, userId: ctx.session.userId! });
    }),
  removeTransferredCourse: userProcedure.input(z.string()).mutation(async ({ ctx, input }) => {
    await db
      .delete(transferredCourse)
      .where(and(eq(transferredCourse.userId, ctx.session.userId!), eq(transferredCourse.courseName, input)));
  }),
  updateTransferredCourse: userProcedure
    .input(z.object({ courseName: z.string(), units: z.number() }))
    .mutation(async ({ ctx, input }) => {
      await db
        .update(transferredCourse)
        .set({ units: input.units })
        .where(
          and(eq(transferredCourse.userId, ctx.session.userId!), eq(transferredCourse.courseName, input.courseName)),
        );
    }),
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
      } else {
        conditions.push(isNull(transferredMisc.courseName));
      }

      if (input.units != null) {
        conditions.push(eq(transferredMisc.units, input.units));
      } else {
        conditions.push(isNull(transferredMisc.units));
      }

      await db.delete(transferredMisc).where(and(...conditions));
    }),
});

export default transferCreditsRouter;
