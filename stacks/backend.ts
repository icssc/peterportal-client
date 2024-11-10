import { FunctionUrlAuthType } from 'aws-cdk-lib/aws-lambda';
import { StackContext, Function } from 'sst/constructs';

export function BackendStack({ stack }: StackContext) {
  const backend = new Function(stack, 'Backend', {
    handler: 'api/src/app.handler',
    memorySize: 256,
    runtime: 'nodejs18.x',
    logRetention: stack.stage === 'prod' ? 'two_years' : 'one_week',
    environment: {
      DATABASE_URL: process.env.DATABASE_URL!,
      SESSION_SECRET: process.env.SESSION_SECRET!,
      PUBLIC_API_URL: process.env.PUBLIC_API_URL!,
      GOOGLE_CLIENT: process.env.GOOGLE_CLIENT!,
      GOOGLE_SECRET: process.env.GOOGLE_SECRET!,
      GRECAPTCHA_SECRET: process.env.GRECAPTCHA_SECRET!,
      PRODUCTION_DOMAIN: process.env.PRODUCTION_DOMAIN!,
      ADMIN_EMAILS: process.env.ADMIN_EMAILS!,
      NODE_ENV: process.env.NODE_ENV ?? 'staging',
    },
  });

  const functionUrl = backend.addFunctionUrl({
    authType: FunctionUrlAuthType.NONE,
  });

  return {
    functionUrl,
  };
}
