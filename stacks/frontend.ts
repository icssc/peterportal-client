import { StaticSite, StackContext } from 'sst/constructs';

export function FrontendStack({ stack }: StackContext) {
  const site = new StaticSite(stack, 'Site', {
    path: './site',
    buildOutput: 'dist',
    buildCommand: 'npm run build',
  });
  return {
    frontendUrl: site.url,
  };
}
