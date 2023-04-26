import { StaticSite, StackContext } from "sst/constructs";
import * as route53 from "aws-cdk-lib/aws-route53";
import * as acm from "aws-cdk-lib/aws-certificatemanager";


export function FrontendStack({stack}: StackContext) {
    const site = new StaticSite(stack, "Site", {
        path: "./site",
        buildOutput: "build",
        buildCommand: "npm run build"
    });

    return {
        frontendUrl: site.url
    }

}