import { StaticSite, StackContext } from "sst/constructs";
import * as route53 from "aws-cdk-lib/aws-route53";
import * as acm from "aws-cdk-lib/aws-certificatemanager";


export function FrontendStack({stack}: StackContext) {
    const hostedZone = route53.HostedZone.fromHostedZoneAttributes(
        this,
        'peterportal-hostedzone',
        {
            zoneName: 'peterportal.org',
            hostedZoneId: process.env.HOSTED_ZONE_ID
        },
    )

    const certificate = acm.Certificate.fromCertificateArn(
        this,
        `api-gateway-cert`,
        process.env.CERTIFICATE_ARN
    )

    const site = new StaticSite(stack, "Site", {
        path: "./site",
        buildOutput: "build",
        buildCommand: "npm run build"
        // customDomain: {
        //     domainName: "test.peterportal.org",
        //     cdk: {
        //         hostedZone,
        //         certificate
        //     }
        // },
    });

    return {
        frontendUrl: site.url
    }

}