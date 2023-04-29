import { FrontendStack } from "./stacks/frontend";
import { BackendStack } from "./stacks/backend";
import dotenv from 'dotenv-flow';

import { App } from "sst/constructs";

dotenv.config();

export default {
    config(_input) {
        return {
            name: "peterportal-client",
            region: "us-west-1",
        };
    },
    stacks(app: App) {
        if (app.stage !== "prod") {
            app.setDefaultRemovalPolicy("destroy");
        }

        app.stack(FrontendStack).stack(BackendStack);
    }
};