import {Api, StackContext, use} from "sst/constructs";
import { FrontendStack } from "./frontend";


export function BackendStack({stack}: StackContext) {
    const { frontendUrl } = use(FrontendStack);

    const api = new Api(stack, "Api", {
        customDomain: {
            domainName: "test.peterportal.org",
            hostedZone: "peterportal.org",
        },
        routes: {
            'ANY /api/{proxy+}': {
                function: {
                    handler: 'api/src/app.handler',
                    memorySize: 512,
                    runtime: 'nodejs14.x',
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
                url: frontendUrl
            }
        },
    });

}
