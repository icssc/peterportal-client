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
// load env
dotenv.config();

// Custom Routes
import authRouter from './controllers/auth';

import { SESSION_LENGTH } from './config/constants';
import { createContext } from './helpers/trpc';
import { appRouter } from './controllers';
import passportInit from './config/passport';

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
  const port = process.env.PORT ?? 8080;
  app.listen(port, () => {
    console.log('Listening on port', port);
  });
}

// export for serverless
export const handler = serverlessExpress({ app });
