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
  return { name: domainName, redirects: domainRedirects };
}

function createTrpcLambdaFunction() {
  const domainName = getDomainConfig().name;
  const productionDomain = `https://${domainName}`;

  const environment = {
    DATABASE_URL: process.env.DATABASE_URL!,
    SESSION_SECRET: process.env.SESSION_SECRET!,
    PUBLIC_API_URL: process.env.PUBLIC_API_URL!,
    OIDC_CLIENT_ID: process.env.OIDC_CLIENT_ID!,
    OIDC_ISSUER_URL: process.env.OIDC_ISSUER_URL!,
    GRECAPTCHA_SECRET: process.env.GRECAPTCHA_SECRET!,
    PRODUCTION_DOMAIN: productionDomain, // Dynamically set based on stage
    PRODUCTION_CLIENT_DOMAIN: productionDomain, // Same as PRODUCTION_DOMAIN (frontend on same domain)
    ADMIN_EMAILS: process.env.ADMIN_EMAILS!,
    NODE_ENV: process.env.NODE_ENV ?? 'staging',
    ANTEATER_API_KEY: process.env.ANTEATER_API_KEY!,
    EXTERNAL_USER_READ_SECRET: process.env.EXTERNAL_USER_READ_SECRET!,
    OTEL_EXPORTER_OTLP_HEADERS: process.env.OTEL_EXPORTER_OTLP_HEADERS!,
    // hardcoded OTEL options
    ...($app.stage === 'prod' && {
      AWS_LAMBDA_EXEC_WRAPPER: '/opt/otel-handler',
      NODE_OPTIONS: '--require @opentelemetry/auto-instrumentations-node/register',
      OTEL_SERVICE_NAME: 'peterportal-backend',
      OTEL_EXPORTER_OTLP_ENDPOINT: 'https://ingress.us-west-2.aws.dash0.com',
    }),
  };

  return new sst.aws.Function('PeterPortal-Backend', {
    handler: 'api/src/app.handler',
    memory: '256 MB',
    runtime: 'nodejs22.x',
    logging: {
      retention: $app.stage === 'prod' ? '2 years' : '1 week',
    },
    environment,
    url: true,
    layers: ['arn:aws:lambda:us-west-1:184161586896:layer:opentelemetry-nodejs-0_13_0:1'],
    nodejs: {
      install: ['@opentelemetry/auto-instrumentations-node'],
    },
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

enum AWSPolicyId {
  // See https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html
  CachingDisabled = '4135ea2d-6df8-44a3-9df3-4b5a84be39ad',
  // See https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html
  AllViewerExceptHostHeader = 'b689b0a8-53d0-40ab-baf2-68738e2966ac',
  // The existing cache policy for PeterPortal's Next.js builds
  OrgNextjsCachePolicy = '0fddd706-8cdb-4835-bf8c-3202baed7dac',
}

/**
 * Creates a CloudFront cache behavior to prevent caching of API requests, forward the host header
 * (used for clients logging in from staging domains since OAUTH urls cannot be added programmatically),
 * and redirect the traffic to be handled by the tRPC handler
 * @param apiOrigin The Origin serving the tRPC API routes
 * @param cloudfrontInjectionFunction The Cloudfront function used to inject headers into API requests,
 * used to add referer headers to logins
 * @returns The behavior to apply to all `/api/*` routes
 */
function createApiCFCacheBehavior(
  apiOrigin: aws.types.input.cloudfront.DistributionOrigin,
  cloudfrontInjectionFunction: aws.cloudfront.Function,
) {
  const behavior: aws.types.input.cloudfront.DistributionOrderedCacheBehavior = {
    pathPattern: '/api/*',
    allowedMethods: ['GET', 'HEAD', 'POST', 'PUT', 'PATCH', 'OPTIONS', 'DELETE'],
    cachedMethods: ['GET', 'HEAD'],
    targetOriginId: apiOrigin.originId,
    viewerProtocolPolicy: 'https-only',
    cachePolicyId: AWSPolicyId.CachingDisabled,
    originRequestPolicyId: AWSPolicyId.AllViewerExceptHostHeader,
    functionAssociations: [
      {
        eventType: 'viewer-request',
        functionArn: cloudfrontInjectionFunction.arn,
      },
    ],
  };
  return behavior;
}

function createNextJsApplication(
  apiCacheBehavior: aws.types.input.cloudfront.DistributionOrderedCacheBehavior,
  apiOrigin: aws.types.input.cloudfront.DistributionOrigin,
) {
  // The Nextjs Site Name must not have spaces; unlike static sites, this name
  // gets prepended in CreatePolicy, so it must meet these requirements:
  // https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreatePolicy.html
  return new sst.aws.Nextjs('PeterPortal-Site', {
    environment: {
      NEXT_PUBLIC_POSTHOG_KEY: process.env.NEXT_PUBLIC_POSTHOG_KEY!,
      NEXT_PUBLIC_POSTHOG_HOST: process.env.NEXT_PUBLIC_POSTHOG_HOST!,
      BACKEND_ROOT_URL: `https://${getDomainConfig().name}/api`,
    },
    cachePolicy: AWSPolicyId.OrgNextjsCachePolicy,
    domain: getDomainConfig(),
    path: './site',
    transform: {
      cdn: (args) => {
        args.origins = $output(args.origins).apply((origins) => [apiOrigin, ...origins]);

        args.orderedCacheBehaviors = $output(args.orderedCacheBehaviors).apply((behaviors) => [
          apiCacheBehavior,
          ...(behaviors ?? []),
        ]);
      },
    },
  });
}

export default $config({
  app(input) {
    return {
      name: 'peterportal-client',
      removal: input?.stage === 'prod' ? 'retain' : 'remove',
      home: 'aws',
      providers: { aws: { region: 'us-west-1' } },
    };
  },

  async run() {
    const lambdaFunction = createTrpcLambdaFunction();

    const apiOrigin = createApiOrigin(lambdaFunction);
    const cloudfrontInjectionFunction = createCloudFrontInjectionFunction();
    const apiCacheBehavior = createApiCFCacheBehavior(apiOrigin, cloudfrontInjectionFunction);

    createNextJsApplication(apiCacheBehavior, apiOrigin);
  },
});
