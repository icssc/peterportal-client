import { StaticSite, StackContext } from "sst/constructs";


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