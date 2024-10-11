/**
 * Configuration for Express server.
 * @module
 */

import express from 'express';
import logger from 'morgan';
import cookieParser from 'cookie-parser';
import passport from 'passport';
import session from 'express-session';
import MongoDBStore from 'connect-mongodb-session';
import dotenv from 'dotenv-flow';
import serverlessExpress from '@vendia/serverless-express';
import * as trpcExpress from '@trpc/server/adapters/express';
import mongoose, { Mongoose } from 'mongoose';
// load env
dotenv.config();

// Configs
import { DB_NAME, COLLECTION_NAMES } from './helpers/mongo';

// Custom Routes
import reviewsRouter from './controllers/reviews';
import roadmapRouter from './controllers/roadmap';
import authRouter from './controllers/auth';

import { SESSION_LENGTH } from './config/constants';
import { createContext } from './helpers/trpc';
import { appRouter } from './controllers';
import passportInit from './config/passport';

// instantiate app
const app = express();

// Setup mongo store for sessions
const mongoStore = MongoDBStore(session);

let store: undefined | MongoDBStore.MongoDBStore;
if (process.env.MONGO_URL) {
  store = new mongoStore({
    uri: process.env.MONGO_URL,
    databaseName: DB_NAME,
    collection: COLLECTION_NAMES.SESSIONS,
  });
} else {
  console.log('MONGO_URL env var is not defined!');
}
// Catch errors
mongoose.connection.on('error', function (error) {
  console.log(error);
});
store?.on('error', function (error) {
  console.log(error);
});
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
    store: store,
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
expressRouter.use('/reviews', reviewsRouter);
expressRouter.use('/roadmap', roadmapRouter);
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
app.use(function (req, res) {
  console.error(req);
  res.status(500).json({ error: `Internal Serverless Error - '${req}'` });
});

export const connect = async () => {
  let conn: null | Mongoose = null;
  const uri = process.env.MONGO_URL;

  if (conn == null && uri) {
    conn = await mongoose.connect(uri!, {
      dbName: DB_NAME,
      serverSelectionTimeoutMS: 5000,
    });
  }
  return conn;
};

let serverlessExpressInstance: ReturnType<typeof serverlessExpress>;
async function setup(event: unknown, context: unknown) {
  await connect();
  serverlessExpressInstance = serverlessExpress({ app });
  return serverlessExpressInstance(event, context);
}
// run local dev server
const NODE_ENV = process.env.NODE_ENV ?? 'development';
if (NODE_ENV === 'development') {
  const port = process.env.PORT ?? 8080;
  connect().then(() => {
    app.listen(port, () => {
      console.log('Listening on port', port);
    });
  });
}

export const handler = async (event: unknown, context: unknown) => {
  if (serverlessExpressInstance) {
    return serverlessExpressInstance(event, context);
  }
  return setup(event, context);
};
// export for serverless
