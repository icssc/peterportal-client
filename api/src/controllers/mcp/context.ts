/**
 * Bridges an authenticated MCP user to a tRPC caller so MCP tools can reuse the existing
 * procedures (`roadmaps.get`, `search.get`, ...) without duplicating any logic.
 * @module
 */

import type { Request, Response } from 'express';
import type { Session, SessionData } from 'express-session';
import { appRouter } from '..';
import type { Context } from '../../helpers/trpc';

/**
 * Build a minimal tRPC {@link Context} for a given user id. Only `session.userId` is read by
 * `userProcedure`; the Anteater-proxying procedures are public and ignore the context.
 */
function buildMcpContext(userId: number): Context {
  const session = { userId } as Session & Partial<SessionData>;
  return {
    req: { headers: {} } as unknown as Request,
    res: {} as unknown as Response,
    session,
  };
}

/** A tRPC caller scoped to the authenticated MCP student. */
export function mcpCaller(userId: number) {
  return appRouter.createCaller(buildMcpContext(userId));
}
