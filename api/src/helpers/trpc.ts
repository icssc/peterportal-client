import * as trpcExpress from '@trpc/server/adapters/express';
import { TRPCError, initTRPC } from '@trpc/server';
import reportsRouter from '../controllers/reports';

export const createContext = ({ req }: trpcExpress.CreateExpressContextOptions) => ({ session: req.session });
type Context = Awaited<ReturnType<typeof createContext>>;
const trpc = initTRPC.context<Context>().create();
export const router = trpc.router;
export const publicProcedure = trpc.procedure;

export const appRouter = trpc.router({
  greeting: publicProcedure.query(({ ctx }) => {
    console.log(ctx.session.passport?.user);
    return 'hello' + ctx.session.passport?.user.name;
  }),
  reports: reportsRouter,
});

// Export only the type of a router!
// This prevents us from importing server code on the client.
export type AppRouter = typeof appRouter;

export const adminProcedure = publicProcedure.use(async (opts) => {
  if (!opts.ctx.session.passport?.admin) throw new TRPCError({ code: 'UNAUTHORIZED', message: 'Not an admin' });

  return opts.next(opts);
});

export const userProcedure = publicProcedure.use(async (opts) => {
  if (!opts.ctx.session.passport) throw new TRPCError({ code: 'UNAUTHORIZED', message: 'Not logged in' });

  return opts.next(opts);
});
