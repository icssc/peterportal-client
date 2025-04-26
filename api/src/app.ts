/**
 * Configuration for Express server.
 * @module
 */

import express, { ErrorRequestHandler } from 'express';
import logger from 'morgan';
import cookieParser from 'cookie-parser';
import passport from 'passport';
import session from 'express-session';
import connectPgSimple from 'connect-pg-simple';
import dotenv from 'dotenv-flow';
import serverlessExpress from '@vendia/serverless-express';
import * as trpcExpress from '@trpc/server/adapters/express';
import { NodeSDK } from '@opentelemetry/sdk-node';
import { ConsoleSpanExporter } from '@opentelemetry/sdk-trace-node';
import { getNodeAutoInstrumentations } from '@opentelemetry/auto-instrumentations-node';
import { PeriodicExportingMetricReader, ConsoleMetricExporter } from '@opentelemetry/sdk-metrics';
import { HttpInstrumentation } from '@opentelemetry/instrumentation-http';
import { ExpressInstrumentation } from '@opentelemetry/instrumentation-express';

// load env
dotenv.config();

// Custom Routes
import authRouter from './controllers/auth';

import { SESSION_LENGTH } from './config/constants';
import { createContext } from './helpers/trpc';
import { appRouter } from './controllers';
import passportInit from './config/passport';
import { OTLPTraceExporter } from '@opentelemetry/exporter-trace-otlp-proto';
import { OTLPMetricExporter } from '@opentelemetry/exporter-metrics-otlp-proto';

// OTEL setup
const setupOTEL = () => {
  if (!process.env.DASH0_TOKEN) {
    console.log('DASH0_TOKEN env var not defined. Telemetry will be logged to standard output.');
  }

  const traceExporter = process.env.DASH0_TOKEN
    ? new OTLPTraceExporter({
        url: 'https://ingress.us-west-2.aws.dash0.com/v1/traces',
        headers: {
          Authorization: process.env.DASH0_TOKEN,
          'Dash0-Dataset': 'peterportal-backend',
        },
      })
    : new ConsoleSpanExporter();

  const exporter = process.env.DASH0_TOKEN
    ? new OTLPMetricExporter({
        url: 'https://ingress.us-west-2.aws.dash0.com/v1/metrics',
        headers: {
          Authorization: process.env.DASH0_TOKEN,
          'Dash0-Dataset': 'peterportal-backend',
        },
      })
    : new ConsoleMetricExporter();

  const sdk = new NodeSDK({
    traceExporter,
    metricReader: new PeriodicExportingMetricReader({
      exporter,
    }),
    instrumentations: [getNodeAutoInstrumentations(), new HttpInstrumentation(), new ExpressInstrumentation()],
  });

  sdk.start();
};

// instantiate app
const app = express();

const PGStore = connectPgSimple(session);

if (!process.env.DATABASE_URL) {
  console.log('DATABASE_URL env var is not defined!');
}
// Setup Passport and Sessions
if (!process.env.SESSION_SECRET) {
  console.log('SESSION_SECRET env var is not defined!');
}
app.use(
  session({
    secret: process.env.SESSION_SECRET ?? 'secret',
    resave: false,
    saveUninitialized: false,
    cookie: { maxAge: SESSION_LENGTH },
    store: new PGStore({
      conString: process.env.DATABASE_URL,
      createTableIfMissing: true,
    }),
  }),
);

if (process.env.GOOGLE_CLIENT && process.env.GOOGLE_SECRET) {
  app.use(passport.initialize());
  app.use(passport.session());
  passportInit();
} else {
  console.log('GOOGLE_CLIENT and/or GOOGLE_SECRET env var(s) not defined! Google login will not be available.');
}

if (!process.env.ANTEATER_API_KEY) {
  console.log('ANTEATER_API_KEY env var is not defined. You will not be able to test search functionality.');
}

/**
 * Configure Express.js Middleware
 */

app.use(express.json());
app.use(logger('dev'));
app.use(cookieParser());
app.use(function (req, res, next) {
  res.header('x-powered-by', 'serverless-express');
  /**
   * on prod/staging, host will be overwritten by lambda function url
   * original host (e.g. peterportal.org or staging-###.peterportal.org) is
   * preserved in x-forwarded-host
   * see stacks/frontend.ts for more info
   */
  if (req.headers['x-forwarded-host']) {
    req.headers.host = req.headers['x-forwarded-host'] as string;
  }
  next();
});

/**
 * Routes - Public
 */

// Enable custom routes
const expressRouter = express.Router();
expressRouter.use('/users/auth', authRouter);
expressRouter.use(
  '/trpc',
  trpcExpress.createExpressMiddleware({
    router: appRouter,
    createContext,
  }),
);

app.use('/api', expressRouter);

/**
 * Error Handler
 */
const errorHandler: ErrorRequestHandler = (err, req, res) => {
  console.error(err);
  res.status(500).json({ message: 'Internal Serverless Error', err });
};
app.use(errorHandler);

// run local dev server
const NODE_ENV = process.env.NODE_ENV ?? 'development';
if (NODE_ENV === 'development') {
  setupOTEL();
  const port = process.env.PORT ?? 8080;
  app.listen(port, () => {
    console.log('Listening on port', port);
  });
}

// export for serverless
export const handler = serverlessExpress({
  app: (() => {
    setupOTEL();
    return app;
  })(),
});
