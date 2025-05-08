import { router, userProcedure } from '../helpers/trpc';
import { db } from '../db';
import { transferredGe } from '../db/schema';
import { eq } from 'drizzle-orm';
import { z } from 'zod';
import { TransferredGE } from '@peterportal/types';

/** @todo complete all routes. We will remove comments after all individual PRs are merged to avoid merge conflicts */
const transferCreditsRouter = router({
  /** @todo add user procedure to get transferred courses below this comment. */
  /** @todo add user procedure to get transferred AP Exams below this comment. */
  /** @todo add user procedure to get transferred GE credits below this comment. */
  getTransferredGEs: userProcedure.query(async ({ ctx }): Promise<TransferredGE[]> => {
    const response = await db
      .select({
        geName: transferredGe.geName,
        numberOfCourses: transferredGe.numberOfCourses,
        units: transferredGe.units,
      })
      .from(transferredGe)
      .where(eq(transferredGe.userId, ctx.session.userId!));
    return response as TransferredGE[];
  }),
  setTransferredGE: userProcedure
    .input(
      z.object({
        GE: z.object({
          geName: z.string(),
          numberOfCourses: z.number(),
          units: z.number(),
        }),
      }),
    )
    .mutation(async ({ input, ctx }) => {
      const { GE } = input as { GE: TransferredGE };
      const userId = ctx.session.userId!;
      await db
        .insert(transferredGe)
        .values({ userId, geName: GE.geName, numberOfCourses: GE.numberOfCourses, units: GE.units })
        .onConflictDoUpdate({
          target: [transferredGe.userId, transferredGe.geName],
          set: { numberOfCourses: GE.numberOfCourses, units: GE.units },
        });
    }),
  /** @todo add user procedure to get transferred untransferred credits below this comment. */
});

export default transferCreditsRouter;
