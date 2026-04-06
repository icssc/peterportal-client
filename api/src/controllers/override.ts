import { override } from '../db/schema';
import { db } from '../db';
import { eq, and } from 'drizzle-orm';
import { router, userProcedure } from '../helpers/trpc';
import z from 'zod';

const overrideRouter = router({
  addOverride: userProcedure
    .input(
      z.object({
        plannerId: z.number(),
        requirement: z.string(),
      }),
    )
    .mutation(async ({ input, ctx }) => {
      const { plannerId, requirement } = input;
      const userId = ctx.session.userId!;

      await db
        .insert(override)
        .values({
          plannerId,
          userId,
          requirement,
        })
        .onConflictDoNothing();
    }),

  deleteOverride: userProcedure
    .input(
      z.object({
        plannerId: z.number(),
        requirement: z.string(),
      }),
    )
    .mutation(async ({ input, ctx }) => {
      const { plannerId, requirement } = input;
      const userId = ctx.session.userId!;

      await db
        .delete(override)
        .where(
          and(eq(override.plannerId, plannerId), eq(override.userId, userId), eq(override.requirement, requirement)),
        );
    }),
});

export default overrideRouter;
