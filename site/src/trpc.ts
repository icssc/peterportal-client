import { createTRPCProxyClient, httpBatchLink } from '@trpc/client';
import { type AppRouter } from '../../api/src/controllers';

const trpc = createTRPCProxyClient<AppRouter>({
  links: [
    httpBatchLink({
      url: '/planner/api/trpc',
    }),
  ],
});

/*
 * "Proper way", if you are brave enough:
 * https://trpc.io/docs/client/react/server-components#5-create-a-trpc-caller-for-server-components
 */

const buildServerTrpcUrl = (headers: Record<string, string>) => {
  const protocol = headers['x-forwarded-proto'] ?? 'http';
  const host = headers['x-forwarded-host'] ?? headers.host ?? 'localhost:3000';

  return `${protocol}://${host}/planner/api/trpc`;
};

export const createServerSideTrpcCaller = (headers: Record<string, string>) => {
  const trpcUrl = buildServerTrpcUrl(headers);
  return createTRPCProxyClient<AppRouter>({ links: [httpBatchLink({ url: trpcUrl, headers })] });
};

export default trpc;
