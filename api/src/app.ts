/**
 * Configuration for Express server.
 * @module
 */

import express from 'express';
import logger from 'morgan';
import cookieParser from 'cookie-parser';
import { graphqlHTTP } from 'express-graphql';
import path from 'path';
import passport from 'passport';
import session from 'express-session';
import MongoDBStore from 'connect-mongodb-session';
import cors from 'cors';
import dotenv from 'dotenv';
import serverless from 'serverless-http';

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
import reportsRouter from './controllers/reports';

// instantiate app
const app = express();

// Setup mongo store for sessions
let mongoStore = MongoDBStore(session);
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
  store: store
}));
app.use(passport.initialize());
app.use(passport.session());
require('./config/passport')

/**
 * Configure Express.js Middleware
 */

app.use(express.json());
app.use(logger('dev'))
app.use(cookieParser());
app.set('view engine', 'ejs');
app.use(express.static(path.join(__dirname, '..', 'public')));
app.use(express.static(path.join(__dirname, '..', 'build')));

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
app.use('/courses', coursesRouter);
app.use('/professors', professorsRouter);
app.use('/schedule', scheduleRouter);
app.use('/reviews', reviewsRouter);
app.use('/users', usersRouter);
app.use('/graphql', graphqlRouter);
app.use('/reports', reportsRouter);
app.use('/about', (req, res) => {
  res.render('about');
});

app.options(`*`, (req, res) => {
  res.status(200).send()
})

app.get(`/test/`, (req, res) => {
  res.status(200).send('Request received')
})

/**
 * Routes - Catch-All
 */

app.get('*', (req, res) => {
  res.sendFile(path.resolve(__dirname, '../build/index.html'));
});

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
exports.handler = serverless(app, { binary: ['image/*'] });