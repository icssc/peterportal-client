/**
 @module UsersRoute
*/

import express, { Request, Response } from 'express';
import passport from 'passport';

let router = express.Router();

/**
 * Get the user's session data
 */
router.get('/', function (req, res, next) {
  res.json(req.session)
});

/**
 * Get whether or not a user is an admin
 */
router.get('/isAdmin', function (req, res, next) {
  // not logged in
  if (!req.session.passport) {
    res.json({ admin: false });
  }
  else {
    res.json({ admin: req.session.passport.admin ? true : false });
  }
});

/**
 * Initiate authentication with Google
 */
router.get('/auth/google',
  function (req, res) {
    
    req.session.returnTo = req.headers.referer;
    
    passport.authenticate('google', {
      scope: ['https://www.googleapis.com/auth/userinfo.profile',
        'https://www.googleapis.com/auth/userinfo.email'],
    })(req, res);
  }
);

/**
 * Callback for Google authentication
 */
router.get('/auth/google/callback', function (req, res) {
  passport.authenticate('google', { failureRedirect: '/', session: true },
    // provides user information to determine whether or not to authenticate
    function (err, user, info) {
      console.log('Logging with Google!')
      if (err) console.log(err);
      else if (!user) console.log('Invalid login data');
      else {
        // manually login
        req.login(user, function (err) {
          if (err) console.log(err);
          else {
            console.log('GOOGLE AUTHORIZED!')
            // check if user is an admin
            let allowedUsers = JSON.parse(process.env.ADMIN_EMAILS)
            if (allowedUsers.includes(user.email)) {
              console.log('AUTHORIZED AS ADMIN');
              req.session.passport!.admin = true;
            }
            successLogin(req, res)
          }
        });
      }
    }
  )(req, res)
}
);

/**
 * Initiate authentication with Facebook
 */
router.get('/auth/facebook',
  function (req, res) {
    req.session.returnTo = req.headers.referer;
    passport.authenticate('facebook', { scope: ['email'] })(req, res);
  });

/**
 * Callback for Facebook authentication
 */
router.get('/auth/facebook/callback',
  passport.authenticate('facebook', { failureRedirect: '/', session: true }),
  successLogin
);

/**
 * Initiate authentication with Github
 */
router.get('/auth/github',
  function (req, res) {
    console.log('START AUTH GITHUB')
    req.session.returnTo = req.headers.referer;
    passport.authenticate('github')(req, res);
  });

/**
 * Callback for Github authentication
 */
router.get('/auth/github/callback',
  function (req, res) {
    passport.authenticate('github', { failureRedirect: '/', session: true },
      // provides user information to determine whether or not to authenticate
      function (err, user, info) {
        console.log('Logging with Github!')
        if (err) console.log(err);
        else if (!user) console.log('Invalid login data');
        else {
          // check if user is an admin
          let allowedUsers = JSON.parse(process.env.GITHUB_ADMIN_USERNAMES)
          if (allowedUsers.includes(user.username)) {
            console.log('GITHUB AUTHORIZED!')
            // manually login
            req.login(user, function (err) {
              if (err) console.log(err);
              else {
                req.session.passport!.admin = true;
                successLogin(req, res)
              }
            });
          }
          else {
            console.log(`INVALID USER! Expected ${allowedUsers}, Got ${user.username}`)
            // failed login
            let returnTo = req.session.returnTo;
            delete req.session.returnTo;
            res.redirect(returnTo!);
          }
        }
      }
    )(req, res)
  }
);

/**
 * Called after successful authentication
 * @param req Express Request Object
 * @param res Express Response Object
 */
function successLogin(req: Request, res: Response) {
  console.log('Logged in', req.user);
  // set the user cookie
  res.cookie('user', req.user);
  // redirect browser to the page they came from
  let returnTo = req.session.returnTo ?? '/';
  delete req.session.returnTo;
  res.redirect(returnTo!);
}

/**
 * Endpoint to logout
 */
router.get('/logout', function (req, res) {
  console.log('Logging out', req.user);
  req.session.destroy(function (err) {
    if (err) console.log(err);
    // clear the user cookie
    res.clearCookie('user');
    res.redirect('back');
  });
});

export default router;