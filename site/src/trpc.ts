import { createTRPCProxyClient, httpBatchLink } from '@trpc/client';
import { type AppRouter } from '../../api/src/controllers';

const trpc = createTRPCProxyClient<AppRouter>({
  links: [
    httpBatchLink({
      url: '/api/trpc',
    }),
  ],
});

/*
 * "Proper way", if you are brave enough:
 * https://trpc.io/docs/client/react/server-components#5-create-a-trpc-caller-for-server-components
 */

export const createServerSideTrpcCaller = (headers: Record<string, string>) => {
  const appRootUrl = 'http://localhost:3000';
  const trpcUrl = appRootUrl + '/api/trpc';
  return createTRPCProxyClient<AppRouter>({ links: [httpBatchLink({ url: trpcUrl, headers })] });
};

export default trpc;
