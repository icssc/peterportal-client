const express = require('express')
const app = express()
const graphqlHTTP = require('express-graphql').graphqlHTTP;
const path = require('path')
const schema = require('./schema');
var passport = require('passport');
var session = require('express-session');
var MongoDBStore = require('connect-mongodb-session')(session);
let { DB_NAME, COLLECTION_NAMES } = require('./helpers/mongo');

// Custom Routes
var coursesRouter = require('./controllers/courses')
var professorsRouter = require('./controllers/professors')
var scheduleRouter = require('./controllers/schedule')
var reviewsRouter = require('./controllers/reviews')
const cors = require('cors');
var usersRouter = require('./controllers/users')

// setup mongo store for sessions
var store = new MongoDBStore({
  uri: process.env.MONGO_URL,
  databaseName: DB_NAME,
  collection: COLLECTION_NAMES.SESSIONS
});
// Catch errors
store.on('error', function (error) {
  console.log(error);
});

app.use(session({
  secret: process.env.SESSION_SECRET,
  resave: false,
  saveUninitialized: false,
  cookie: { maxAge: 1000 * 60 * 60 * 24 },
  store: store
}));
app.use(passport.initialize());
app.use(passport.session());
require("./config/passport.js")

app.set('view engine', 'ejs');
app.use(express.static(path.join(__dirname, 'public')));

app.use(express.static(path.join(__dirname, 'build')));

/**
 * Inference:
 * 
 * When "inference" in serverless.yml is "true", the Serverless Framework will attempt to
 * initialize this application on every deployment, extract its endpoints and generate an OpenAPI specification (super cool)
 * However, the Framework won't have access to the environment variables, causing some things to crash on load.
 * That is why some things have try/catch blocks around them when they are required.
 */


/**
 * Configure Express.js Middleware
 */

// Enable CORS
app.use(function (req, res, next) {
  res.header('Access-Control-Allow-Origin', '*')
  res.header('Access-Control-Allow-Methods', '*')
  res.header('Access-Control-Allow-Headers', '*')
  res.header('x-powered-by', 'serverless-express')
  next()
})

app.use(cors());

// Enable JSON use
app.use(express.json())

// Since Express doesn't support error handling of promises out of the box,
// this handler enables that
const asyncHandler = fn => (req, res, next) => {
  return Promise
    .resolve(fn(req, res, next))
    .catch(next);
};


app.use("/courses", coursesRouter);
app.use("/professors", professorsRouter);
app.use("/schedule", scheduleRouter);
app.use("/reviews", reviewsRouter);
app.use("/users", usersRouter);

app.use('/about', (req, res) => {
  res.render('about');
});

//sakshi for graphql
app.use('/graphql', graphqlHTTP({
  schema,
  graphiql: true
}));

/**
 * Routes - Public
 */

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
  res.sendFile(path.resolve(__dirname, './build/index.html'));
});

/**
 * Error Handler
 */
app.use(function (err, req, res, next) {
  console.error(err)
  res.status(500).json({ error: `Internal Serverless Error - "${err.message}"` })
})

module.exports = app