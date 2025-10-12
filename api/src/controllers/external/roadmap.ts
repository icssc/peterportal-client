import { TRPCError } from '@trpc/server';
import { publicProcedure, router } from '../../helpers/trpc';
import { db } from '../../db';
import { planner, user } from '../../db/schema';
import { eq } from 'drizzle-orm';
import { z } from 'zod';

const externalRoadmapsRouter = router({
  getByGoogleID: publicProcedure.input(z.object({ googleUserId: z.string() })).query(async ({ input, ctx }) => {
    const authToken = ctx.req.headers.authorization;
    if (authToken !== 'Bearer ' + process.env.EXTERNAL_USER_READ_SECRET) {
      throw new TRPCError({ code: 'UNAUTHORIZED' });
    }

    const planners = await db
      .select({ name: planner.name, content: planner.years })
      .from(planner)
      .innerJoin(user, eq(planner.userId, user.id))
      .where(eq(user.googleId, input.googleUserId))
      .orderBy(planner.id);

    return planners;
  }),
});

export default externalRoadmapsRouter;
