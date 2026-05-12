import { TRPCError } from '@trpc/server';
import { publicProcedure, router } from '../../helpers/trpc';
import { account, user } from '../../db/schema';
import { eq, or } from 'drizzle-orm';
import { z } from 'zod';
import { db } from '../../db';
import { queryGetPlanners } from '../../helpers/roadmap';

const externalRoadmapsRouter = router({
  getByGoogleID: publicProcedure.input(z.object({ googleUserId: z.string() })).query(async ({ input, ctx }) => {
    const authToken = ctx.req.headers.authorization;
    if (authToken !== `Bearer ${process.env.EXTERNAL_USER_READ_SECRET}`) {
      throw new TRPCError({ code: 'UNAUTHORIZED' });
    }

    const idLegacy = input.googleUserId;
    const idPrefixed = `google_${idLegacy}`;

    const where = or(eq(account.providerAccountId, idPrefixed), eq(account.providerAccountId, idLegacy));
    const [matchedAccount] = await db.select().from(account).where(where);
    if (!matchedAccount) return [];

    return await queryGetPlanners(eq(user.id, matchedAccount.userId));
  }),
});

export default externalRoadmapsRouter;
