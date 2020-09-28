var express = require('express');
var passport = require("passport");
var {executeQuery, escape} = require("../config/database.js")
var router = express.Router();
const path = require('path')
const dotenv = require('dotenv');
dotenv.config({ path: path.resolve(__dirname, '../.env') });

// get the logged in user
router.get('/', function(req, res, next) {
  res.json(req.session)
});

// get the name of the logged in user
router.get('/getName', function(req, res, next) {
  console.log("User:", req.user);
  res.json( {name: (req.user ? req.user.name: undefined), 
    picture: (req.user ? req.user.picture: undefined) });
});

// get whether or not a user is logged in
router.get('/loggedIn', function(req, res, next) {
  res.json({status: req.user ? true: false});
});

// get whether or not a user is an admin
router.get('/isAdmin', function(req, res, next) {
  res.json({admin: req.session.passport.admin ? true: false});
});

// route to login with google
router.get("/auth/google",
  function(req, res) {
    req.session.returnTo = req.headers.referer;
    passport.authenticate("google", { scope: ["profile", "email"]})(req, res);
  }
);

// google callback
router.get("/auth/google/callback",
  passport.authenticate("google", { failureRedirect: "/", session: true }),
  successLogin
);

// route to login with facebook
router.get('/auth/facebook',
  function(req, res){
    req.session.returnTo = req.headers.referer;
    passport.authenticate('facebook', { scope : ['email'] })(req, res);
});

// facebook callback
router.get('/auth/facebook/callback',
  passport.authenticate('facebook', { failureRedirect: '/', session: true }),
  successLogin
);

// route to login with github
router.get('/auth/github',
  function(req, res){
    req.session.returnTo = req.headers.referer;
    passport.authenticate('github')(req, res);
});

// github callback
router.get('/auth/github/callback',
  function(req, res){
    passport.authenticate('github', { failureRedirect: '/', session: true },
      // provides user information to determine whether or not to authenticate
      function (err, user, info){
        if (err) console.log(err);
        else if (!user) console.log("Invalid login data");
        else{
          // check if user is an admin
          allowedUsers = JSON.parse(process.env.GITHUB_ADMIN_USERNAMES)
          if (allowedUsers.includes(user.username))
          {
            // manually login
            req.login(user, function(err) {
              if (err) console.log(err);
              else
              {
                req.session.passport.admin = true;
                successLogin(req, res)
              }
            });
          }
          else{
            // failed login
            let returnTo = req.session.returnTo;
            delete req.session.returnTo;    
            res.redirect(returnTo);
          }
        }
      }
    )(req, res)
  }
);

// call after successful authentication
function successLogin(req, res){
  // check if user is in the database
  let sql = `SELECT * FROM users AS r WHERE r.email = ${escape(req.user.email)}`
  executeQuery(sql, function(results) {
    // if not in the database
    if (results.length == 0) {
      // add them
      let sql = `INSERT INTO users (email, full_name) VALUES('${req.user.email}', '${req.user.name}')`
      executeQuery(sql, function(results) {
        console.log(results)
      });
    } else {
      console.log(results)
      req.user['userID'] = results[0].user_id;
      
    }
  });
  // redirect browser to the page they came from
  let returnTo = req.session.returnTo;
  delete req.session.returnTo;    
  res.redirect(returnTo);
}

// endpoint to logout
router.get('/logout', function(req, res){
  req.session.destroy(function (err) {
    if (err) console.log(err)
    res.redirect('back');
  });
});

module.exports = router;
