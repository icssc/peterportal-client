import { FrontendStack } from './stacks/frontend';
import { BackendStack } from './stacks/backend';
import dotenv from 'dotenv-flow';

import { App } from 'sst/constructs';

dotenv.config();

export default {
  config() {
    return {
      name: 'peterportal-client',
      region: 'us-west-1',
    };
  },
  stacks(app: App) {
    if (app.stage !== 'prod') {
      app.setDefaultRemovalPolicy('destroy');
    }

    app
      .stack(BackendStack, { stackName: `${app.name}-${app.stage}-backend` })
      .stack(FrontendStack, { stackName: `${app.name}-${app.stage}-frontend` });
  },
};
