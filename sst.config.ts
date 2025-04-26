// eslint-disable-next-line @typescript-eslint/triple-slash-reference
/// <reference path="./.sst/platform/config.d.ts" />

function getDomainConfig() {
  let domainName: string;
  let domainRedirects: string[] | undefined;
  if ($app.stage === 'prod') {
    domainName = 'peterportal.org';
    domainRedirects = ['www.peterportal.org'];
  } else if ($app.stage === 'dev') {
    domainName = 'dev.peterportal.org';
  } else if ($app.stage.match(/^staging-(\d+)$/)) {
    // check if stage is like staging-###
    domainName = `${$app.stage}.peterportal.org`;
  } else {
    throw new Error('Invalid stage');
  }
  return { domainName, domainRedirects };
}

function createLambdaFunction() {
  const environment = {
    DATABASE_URL: process.env.DATABASE_URL!,
    SESSION_SECRET: process.env.SESSION_SECRET!,
    PUBLIC_API_URL: process.env.PUBLIC_API_URL!,
    GOOGLE_CLIENT: process.env.GOOGLE_CLIENT!,
    GOOGLE_SECRET: process.env.GOOGLE_SECRET!,
    GRECAPTCHA_SECRET: process.env.GRECAPTCHA_SECRET!,
    PRODUCTION_DOMAIN: process.env.PRODUCTION_DOMAIN!,
    ADMIN_EMAILS: process.env.ADMIN_EMAILS!,
    NODE_ENV: process.env.NODE_ENV ?? 'staging',
    ANTEATER_API_KEY: process.env.ANTEATER_API_KEY!,
    EXTERNAL_USER_READ_SECRET: process.env.EXTERNAL_USER_READ_SECRET!,
    DASH0_TOKEN: process.env.DASH0_TOKEN!,
  };

  return new sst.aws.Function('PeterPortal Backend', {
    handler: 'api/src/app.handler',
    memory: '256 MB',
    runtime: 'nodejs22.x',
    logging: {
      retention: $app.stage === 'prod' ? '2 years' : '1 week',
    },
    environment,
    url: true,
  });
}

/**
 * forwards host since lambda function url overwrites host (x-forwarded-host is recovered in api/app.ts)
 * encodes querystryings since cloudfront can't support "/" in querystring otherwise
 * @returns cloudfront function
 */
const createCloudFrontInjectionFunction = () =>
  new aws.cloudfront.Function(`${$app.name}-${$app.stage}-CloudFrontFunction`, {
    runtime: 'cloudfront-js-2.0',
    // this code is copy/pasted from an SST sveltekit component, forwards host and encodes query string
    code: `
      function handler(event) {
        var request = event.request;
        request.headers["x-forwarded-host"] = request.headers.host;
        for (var key in request.querystring) {
          if (key.includes("/")) {
            request.querystring[encodeURIComponent(key)] = request.querystring[key];
            delete request.querystring[key];
          }
        }
        return request;
      }
    `,
  });

function createApiOrigin(lambdaFunction: sst.aws.Function): aws.types.input.cloudfront.DistributionOrigin {
  return {
    domainName: lambdaFunction.url.apply((url) => new URL(url).hostname),
    originId: 'api',
    customOriginConfig: {
      httpPort: 80,
      httpsPort: 443,
      originProtocolPolicy: 'https-only',
      originSslProtocols: ['TLSv1.2'],
    },
  };
}

function createStaticSite(
  domainName: string,
  domainRedirects: string[] | undefined,
  apiOrigin: aws.types.input.cloudfront.DistributionOrigin,
  cloudfrontInjectionFunction: aws.cloudfront.Function,
) {
  return new sst.aws.StaticSite('PeterPortal Site', {
    domain: {
      name: domainName,
      redirects: domainRedirects,
    },
    path: './site',
    build: {
      command: 'pnpm build',
      output: 'dist',
    },
    transform: {
      cdn: (args) => {
        args.origins = $output(args.origins).apply((origins) => [...origins, apiOrigin]);
        args.orderedCacheBehaviors = [
          {
            pathPattern: '/api/*',
            allowedMethods: ['GET', 'HEAD', 'POST', 'PUT', 'PATCH', 'OPTIONS', 'DELETE'],
            cachedMethods: ['GET', 'HEAD'],
            targetOriginId: apiOrigin.originId,
            viewerProtocolPolicy: 'https-only',
            cachePolicyId: '4135ea2d-6df8-44a3-9df3-4b5a84be39ad', // caching disabled policy: https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html
            originRequestPolicyId: 'b689b0a8-53d0-40ab-baf2-68738e2966ac', // all viewer except host header policy: https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html
            functionAssociations: [
              {
                eventType: 'viewer-request',
                functionArn: cloudfrontInjectionFunction.arn,
              },
            ],
          },
        ];
      },
    },
    environment: {
      VITE_PUBLIC_POSTHOG_KEY: process.env.VITE_PUBLIC_POSTHOG_KEY!,
      VITE_PUBLIC_POSTHOG_HOST: process.env.VITE_PUBLIC_POSTHOG_HOST!,
    },
  });
}

export default $config({
  app(input) {
    return {
      name: 'peterportal-client',
      removal: input?.stage === 'prod' ? 'retain' : 'remove',
      home: 'aws',
      providers: {
        aws: {
          region: 'us-west-1',
        },
      },
    };
  },

  async run() {
    const { domainName, domainRedirects } = getDomainConfig();
    const lambdaFunction = createLambdaFunction();

    const apiOrigin = createApiOrigin(lambdaFunction);
    const cloudfrontInjectionFunction = createCloudFrontInjectionFunction();

    createStaticSite(domainName, domainRedirects, apiOrigin, cloudfrontInjectionFunction);
  },
});
