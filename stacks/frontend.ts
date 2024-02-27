import { StaticSite, StackContext, use } from 'sst/constructs';
import { FunctionUrlOrigin } from 'aws-cdk-lib/aws-cloudfront-origins';
import { BackendStack } from './backend';
import { AllowedMethods, CachePolicy, ViewerProtocolPolicy } from 'aws-cdk-lib/aws-cloudfront';
import { IFunctionUrl } from 'aws-cdk-lib/aws-lambda';

export function FrontendStack({ app, stack }: StackContext) {
  const { functionUrl } = use(BackendStack);
  // this functionurl cast should be safe
  const apiOrigin = new FunctionUrlOrigin(functionUrl as unknown as IFunctionUrl);

  let domainName: string;
  if (app.stage === 'prod') {
    domainName = 'peterportal.org';
  } else if (app.stage === 'dev') {
    domainName = 'dev.peterportal.org';
  } else if (app.stage.match(/^staging-(\d+)$/)) {
    // check if stage is like staging-###
    domainName = `${app.stage}.peterportal.org`;
  } else {
    throw new Error('Invalid stage');
  }

  new StaticSite(stack, 'Site', {
    customDomain: {
      domainName: domainName,
      domainAlias: app.stage === 'prod' ? 'www.peterportal.org' : undefined,
      hostedZone: 'peterportal.org',
    },
    path: './site',
    buildOutput: 'dist',
    buildCommand: 'npm run build',
    cdk: {
      distribution: {
        additionalBehaviors: {
          'api/*': {
            origin: apiOrigin,
            viewerProtocolPolicy: ViewerProtocolPolicy.HTTPS_ONLY,
            cachePolicy: CachePolicy.CACHING_DISABLED,
            allowedMethods: AllowedMethods.ALLOW_ALL,
          },
        },
      },
    },
  });
}
