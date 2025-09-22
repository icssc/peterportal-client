import { TRPCError } from '@trpc/server';
import { publicProcedure, router } from '../../helpers/trpc';
import { user } from '../../db/schema';
import { eq } from 'drizzle-orm';
import { z } from 'zod';
import { queryGetPlanners } from '../../helpers/roadmap';

const externalRoadmapsRouter = router({
  getByGoogleID: publicProcedure.input(z.object({ googleUserId: z.string() })).query(async ({ input, ctx }) => {
    const authToken = ctx.req.headers.authorization;
    if (authToken !== 'Bearer ' + process.env.EXTERNAL_USER_READ_SECRET) {
      throw new TRPCError({ code: 'UNAUTHORIZED' });
    }

    const planners = await queryGetPlanners(eq(user.googleId, input.googleUserId));

    return planners;
  }),
});

export default externalRoadmapsRouter;
