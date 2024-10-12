import express, { Request, Response } from 'express';
import passport from 'passport';
import { SESSION_LENGTH } from '../config/constants';
import { User } from '@peterportal/types';

const router = express.Router();

/**
 * Called after successful authentication
 * @param req Express Request Object
 * @param res Express Response Object
 */
function successLogin(req: Request, res: Response) {
  console.log('Logged in', req.user);
  // set the user cookie
  res.cookie('user', req.user, {
    maxAge: SESSION_LENGTH,
  });
  // redirect browser to the page they came from
  const returnTo = req.session.returnTo ?? '/';
  delete req.session.returnTo;
  res.redirect(returnTo!);
}

/**
 * Initiate authentication with Google
 */
router.get('/google', function (req, res) {
  req.session.returnTo = req.headers.referer;
  passport.authenticate('google', {
    scope: ['https://www.googleapis.com/auth/userinfo.profile', 'https://www.googleapis.com/auth/userinfo.email'],
    state: req.headers.host,
  })(req, res);
});

/**
 * Callback for Google authentication
 */
router.get('/google/callback', function (req, res) {
  const returnTo = req.session.returnTo;
  const host: string = req.query.state as string;
  // all staging auths will redirect their callback to prod since all callback URLs must be registered
  // with google cloud for security reasons and it isn't feasible to register the callback URLs for all
  // staging instances
  // if we are not on a staging instance (on prod or local) but original host is a staging instance, redirect back to host
  if (host.startsWith('staging-') && !req.headers.host?.startsWith('staging')) {
    // req.url doesn't include /api/users part, only /auth/google/callback? and whatever params after that
    res.redirect('https://' + host + '/api/users' + req.url);
    return;
  }
  passport.authenticate(
    'google',
    { failureRedirect: '/', session: true },
    // provides user information to determine whether or not to authenticate
    function (err: Error, user: User | false | null) {
      if (err) console.error(err);
      else if (!user) console.error('Invalid login data');
      else {
        // manually login
        req.login(user, function (err) {
          if (err) console.error(err);
          else {
            // check if user is an admin
            const allowedUsers = JSON.parse(process.env.ADMIN_EMAILS ?? '[]');
            if (allowedUsers.includes(user.email)) {
              console.log('AUTHORIZED AS ADMIN');
              req.session.passport!.admin = true;
            }
            req.session.returnTo = returnTo;
            successLogin(req, res);
          }
        });
      }
    },
  )(req, res);
});

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
