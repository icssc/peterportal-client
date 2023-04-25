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
import cors from 'cors';
import dotenv from 'dotenv-flow';
import serverlessExpress from '@vendia/serverless-express';
import nocache from 'nocache';

console.log('Starting server...')

// load env
dotenv.config();

// Configs
import { DB_NAME, COLLECTION_NAMES } from './helpers/mongo';

// Custom Routes
import coursesRouter from './controllers/courses';
import professorsRouter from './controllers/professors';
import scheduleRouter from './controllers/schedule';
import reviewsRouter from './controllers/reviews';
import usersRouter from './controllers/users';
import graphqlRouter from './controllers/graphql';
import roadmapRouter from './controllers/roadmap';
import reportsRouter from './controllers/reports';

// instantiate app
const app = express();

// Setup mongo store for sessions
let mongoStore = MongoDBStore(session);

if (process.env.MONGO_URL) {
  var store = new mongoStore({
    uri: process.env.MONGO_URL,
    databaseName: DB_NAME,
    collection: COLLECTION_NAMES.SESSIONS
  });
  // Catch errors
  store.on('error', function (error) {
    console.log(error);
  });
  // Setup Passport and Sesssions
  app.use(session({
    secret: process.env.SESSION_SECRET,
    resave: false,
    saveUninitialized: false,
    cookie: { maxAge: 1000 * 60 * 60 * 24 },
    store: store,
  }));
  app.use(passport.initialize());
  app.use(passport.session());
  require('./config/passport')
}
else {
  console.log('MONGO_URL env var is not defined!')
}

/**
 * Configure Express.js Middleware
 */


app.use(express.json());
app.use(logger('dev'))
app.use(cookieParser());
app.set('view engine', 'ejs');

// Enable CORS
app.use(function (req, res, next) {
  res.header('Access-Control-Allow-Origin', '*')
  res.header('Access-Control-Allow-Methods', '*')
  res.header('Access-Control-Allow-Headers', '*')
  res.header('x-powered-by', 'serverless-express')
  next()
})
app.use(cors());

/**
 * Routes - Public
 */

// Enable custom routes
const router = express.Router();
router.use('/courses', coursesRouter);
router.use('/professors', professorsRouter);
router.use('/schedule', scheduleRouter);
router.use('/reviews', reviewsRouter);
router.use('/users', usersRouter);
router.use('/graphql', graphqlRouter);
router.use('/roadmap', roadmapRouter);
router.use('/reports', reportsRouter);
app.use('/api', router);

app.options(`*`, (req, res) => {
  res.status(200).send()
})

app.get(`/test`, (req, res) => {
  res.status(200).send('Hello World!')
})

/**
 * Routes - Catch-All and redirect to React frontend. Do not cache index.html.
 */
app.use(nocache());

/**
 * Error Handler
 */
app.use(function (req, res, next) {
  console.error(req)
  res.status(500).json({ error: `Internal Serverless Error - '${req}'` })
})

// export for local dev
export default app
// export for serverless
export const handler = serverlessExpress({app});
