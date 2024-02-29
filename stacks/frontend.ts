import { StaticSite, StackContext, use } from 'sst/constructs';
import { Fn } from 'aws-cdk-lib/core';
import { HttpOrigin } from 'aws-cdk-lib/aws-cloudfront-origins';
import {
  Function,
  FunctionCode,
  AllowedMethods,
  CachePolicy,
  ViewerProtocolPolicy,
  FunctionEventType,
  OriginRequestPolicy,
} from 'aws-cdk-lib/aws-cloudfront';
import { BackendStack } from './backend';

export function FrontendStack({ app, stack }: StackContext) {
  const { functionUrl } = use(BackendStack);
  const apiOrigin = new HttpOrigin(Fn.parseDomainName(functionUrl.url));

  /**
   * the host will be changed to the lambda function url when the cloudfront
   * distribution forwards the request to the lambda function
   * we want to preserve the origial host so we put it in a different header
   * by using this cloudfront function
   *
   * this also fixes an issue with query strings
   * from sst source code:
   * 'Note: form action requests contain "/" in request query string
   * ie. POST request with query string "?/action"
   * CloudFront does not allow query string with "/". It needs to be encoded.'
   */
  const forwardHostFunction = new Function(stack, 'CloudFrontFunction', {
    // this code is copy/pasted from a sveltekit site deployed with SST
    code: FunctionCode.fromInline(`
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
    `),
  });

  let domainName: string;
  // let domainAlias: string | undefined;
  if (app.stage === 'prod') {
    domainName = 'peterportal.org';
    // domainAlias = 'www.peterportal.org';
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
      // domainAlias: domainAlias,
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
            allowedMethods: AllowedMethods.ALLOW_ALL, // need to allow POST, DELETE, etc.
            cachePolicy: CachePolicy.CACHING_DISABLED, // don't cache api requests
            originRequestPolicy: OriginRequestPolicy.ALL_VIEWER_EXCEPT_HOST_HEADER,
            functionAssociations: [
              {
                function: forwardHostFunction,
                eventType: FunctionEventType.VIEWER_REQUEST,
              },
            ],
          },
        },
      },
    },
  });
}
