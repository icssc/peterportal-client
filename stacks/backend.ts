import {Api, StackContext, use} from "sst/constructs";
import { FrontendStack } from "./frontend";


export function BackendStack({app, stack}: StackContext) {
    const { frontendUrl } = use(FrontendStack);

    let domainName;
    if (app.stage === 'prod') {
        domainName = 'peterportal.org'
    }
    else if (app.stage === 'dev') {
        domainName = 'dev.peterportal.org'
    }
    else if (!isNaN(app.stage as any)) {
        const prNum = parseInt(app.stage)
        domainName = `staging-${prNum}.peterportal.org`
    }
    else {
        throw new Error('Invalid stage')
    }

    const api = new Api(stack, "Api", {
        customDomain: {
            domainName: domainName,
            hostedZone: "peterportal.org",
        },
        routes: {
            'ANY /api/{proxy+}': {
                function: {
                    handler: 'api/src/app.handler',
                    memorySize: 256,
                    runtime: 'nodejs18.x',
                    logRetention: stack.stage === 'prod' ? 'two_years' : 'one_week',
                    environment: {
                        MONGO_URL: process.env.MONGO_URL,
                        SESSION_SECRET: process.env.SESSION_SECRET,
                        PUBLIC_API_URL: process.env.PUBLIC_API_URL,
                        PUBLIC_API_GRAPHQL_URL: process.env.PUBLIC_API_GRAPHQL_URL,
                        GOOGLE_CLIENT: process.env.GOOGLE_CLIENT,
                        GOOGLE_SECRET: process.env.GOOGLE_SECRET,
                        PRODUCTION_DOMAIN: process.env.PRODUCTION_DOMAIN,
                        ADMIN_EMAILS: process.env.ADMIN_EMAILS
                    }
                }
            },
            $default: {
                type: "url",
                url: frontendUrl ?? "peterportal.org" // when removing the frontend stack, frontendUrl will be undefined
            }
        },
    });
}
