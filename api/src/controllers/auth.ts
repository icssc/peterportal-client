import express, { Request, Response } from 'express';
import passport from 'passport';
import { SESSION_LENGTH } from '../config/constants';
import { PassportUser } from '@peterportal/types';
import { db } from '../db';
import { user } from '../db/schema';

const router = express.Router();

/**
 * Called after successful authentication
 * @param req Express Request Object
 * @param res Express Response Object
 */
async function successLogin(req: Request, res: Response) {
  const {
    email,
    name,
    id: googleId,
    picture,
  } = req.user as { email: string; id: string; name: string; picture: string };
  // upsert user data in db
  const userData = await db
    .insert(user)
    .values({ googleId, name, email, picture })
    .onConflictDoUpdate({ target: user.googleId, set: { name, email, picture } })
    .returning();
  res.cookie('user', true, {
    maxAge: SESSION_LENGTH,
  });
  req.session.userId = userData[0].id;
  // redirect browser to the page they came from
  const returnTo = req.session.returnTo ?? '/';
  delete req.session.returnTo;
  res.redirect(returnTo!);
}

/**
 * Initiate authentication with Google
 */
router.get('/google', function (req, res) {
  req.session.returnTo = 'http://localhost:5173'; //req.headers.referer;
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
    // req.url doesn't include /api/users/auth part, only /google/callback? and whatever params after that
    res.redirect(`https://${host}/api/users/auth${req.url}`);
    return;
  }
  passport.authenticate(
    'google',
    { failureRedirect: '/', session: true },
    // provides user information to determine whether or not to authenticate
    function (err: Error, user: PassportUser | false | null) {
      if (err) return console.error(err);
      if (!user) return console.error('Invalid login data');
      // manually login
      req.login(user, function (err) {
        if (err) return console.error(err);
        // check if user is an admin
        const allowedUsers = JSON.parse(process.env.ADMIN_EMAILS ?? '[]');
        if (allowedUsers.includes(user.email)) {
          req.session.isAdmin = true;
        }
        req.session.returnTo = returnTo;
        successLogin(req, res);
      });
    },
  )(req, res);
});

/**
 * Endpoint to logout
 */
router.get('/logout', function (req, res) {
  req.session.destroy(function (err) {
    if (err) console.error(err);
    // clear the user cookie
    res.clearCookie('user');
    res.redirect('back');
  });
});

export default router;
