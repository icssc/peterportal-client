import { fetchRequestHandler } from '@trpc/server/adapters/fetch';

import { appRouter } from '../controllers';
import { createContext } from '../helpers/trpc';

const TRPC_ENDPOINT = '/api/trpc';

export const trpcRequestHandler = (request: Request) =>
  fetchRequestHandler({
    endpoint: TRPC_ENDPOINT,
    req: request,
    router: appRouter,
    createContext: () => createContext(request),
  });

export const GET = trpcRequestHandler;
export const POST = trpcRequestHandler;
