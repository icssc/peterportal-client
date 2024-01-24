/**
 @module UsersRoute
*/

import express, { Request, Response } from 'express';
import passport from 'passport';
import { COLLECTION_NAMES, addDocument, getCollection, getDocuments, updateDocument } from '../helpers/mongo';
import { SESSION_LENGTH } from '../config/constants';

let router = express.Router();

/**
 * Get the user's session data
 */
router.get('/', function (req, res, next) {
  res.json(req.session);
});

router.get('/preferences', (req, res) => {
  if (!req.session.passport) {
    res.json({ error: 'Must be logged in to get preferences.' });
    return;
  }

  const userID = req.session.passport.user.id;

  getDocuments(COLLECTION_NAMES.PREFERENCES, { _id: userID }).then((preferences) => {
    if (preferences.length > 0) {
      res.json(preferences[0]);
    } else {
      res.json({ error: 'No preferences found' });
    }
  });
});

interface UserPreferences {
  theme?: string;
}

router.post('/preferences', async (req, res) => {
  if (!req.session.passport) {
    res.json({ error: 'Must be logged in to get preferences.' });
    return;
  }

  const userID = req.session.passport.user.id;

  // make user's preference doc if it doesn't exist
  const preferencesCollection = await getCollection(COLLECTION_NAMES.PREFERENCES);
  if ((await preferencesCollection.find({ _id: userID }).count()) == 0) {
    await addDocument(COLLECTION_NAMES.PREFERENCES, { _id: userID });
  }

  // grab valid preferences from request body
  const preferences: UserPreferences = {};
  if (req.body.theme) {
    preferences.theme = req.body.theme;
  }

  // set the preferences
  await updateDocument(COLLECTION_NAMES.PREFERENCES, { _id: userID }, { $set: preferences });

  // echo back body
  res.json(req.body);
});

/**
 * Get whether or not a user is an admin
 */
router.get('/isAdmin', function (req, res, next) {
  // not logged in
  if (!req.session?.passport) {
    res.json({ admin: false });
  } else {
    res.json({ admin: req.session.passport.admin });
  }
});

/**
 * Initiate authentication with Google
 */
router.get('/auth/google', function (req, res) {
  req.session.returnTo = req.headers.referer;
  passport.authenticate('google', {
    scope: ['https://www.googleapis.com/auth/userinfo.profile', 'https://www.googleapis.com/auth/userinfo.email'],
    state: req.headers.host,
  })(req, res);
});

/**
 * Callback for Google authentication
 */
router.get('/auth/google/callback', function (req, res) {
  const returnTo = req.session.returnTo;
  let host: string = req.query.state as string;
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
    function (err, user, info) {
      if (err) console.log(err);
      else if (!user) console.log('Invalid login data');
      else {
        // manually login
        req.login(user, function (err) {
          if (err) console.log(err);
          else {
            // check if user is an admin
            let allowedUsers = JSON.parse(process.env.ADMIN_EMAILS);
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
