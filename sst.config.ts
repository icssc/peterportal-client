import { SSTConfig } from "sst";
import { FrontendStack } from "./stacks/frontend";
import { BackendStack } from "./stacks/backend";
import dotenv from 'dotenv-flow';

dotenv.config();


export default {
    config(_input) {
        return {
            name: "peterportal-test",
            region: "us-east-1",
        };
    },
    stacks(app) {
        app.stack(FrontendStack).stack(BackendStack)
    }
};