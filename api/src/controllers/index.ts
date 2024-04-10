import { publicProcedure, router } from '../helpers/trpc';
import reportsRouter from './reports';
import usersRouter from './users';

export const appRouter = router({
  greeting: publicProcedure.query(({ ctx }) => {
    console.log(ctx.session.passport?.user);
    return 'hello ' + ctx.session.passport?.user.name;
  }),
  reports: reportsRouter,
  users: usersRouter,
});

// Export only the type of a router!
// This prevents us from importing server code on the client.
export type AppRouter = typeof appRouter;
