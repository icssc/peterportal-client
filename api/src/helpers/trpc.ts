import * as trpcExpress from '@trpc/server/adapters/express';
import { TRPCError, initTRPC } from '@trpc/server';
import type { Session, SessionData } from 'express-session';

interface CustomRequest extends Request {
  // session: {
  //   passport: PassportData;
  // }
  session: Session & Partial<SessionData>;
}

export const createContext = (req: CustomRequest, res: trpcExpress.CreateExpressContextOptions['res']) => ({
  req,
  session: req.session,
  res,
});

type Context = Awaited<ReturnType<typeof createContext>>;
const trpc = initTRPC.context<Context>().create();
export const router = trpc.router;
export const publicProcedure = trpc.procedure;

export const adminProcedure = publicProcedure.use(async (opts) => {
  if (!opts.ctx.session.passport?.admin) throw new TRPCError({ code: 'UNAUTHORIZED', message: 'Not an admin' });

  return opts.next(opts);
});

export const userProcedure = publicProcedure.use(async (opts) => {
  if (!opts.ctx.session.passport) throw new TRPCError({ code: 'UNAUTHORIZED', message: 'Not logged in' });

  return opts.next(opts);
});
