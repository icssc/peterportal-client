/**
 @module PassportConfig
*/

import { User } from 'express-session';
import passport from 'passport';
import { OAuth2Strategy as GoogleStrategy } from 'passport-google-oauth';

export default function passportInit() {
  passport.serializeUser(function (user, done) {
    done(null, user);
  });

  passport.deserializeUser(function (user: false | User | null | undefined, done) {
    done(null, user);
  });

  /**
   * Configuration for Google Strategy
   */
  passport.use(
    new GoogleStrategy(
      {
        clientID: process.env.GOOGLE_CLIENT,
        clientSecret: process.env.GOOGLE_SECRET,
        callbackURL: process.env.PRODUCTION_DOMAIN + '/api/users/auth/google/callback',
      },
      function (accessToken, refreshToken, profile, done) {
        let email = '';
        // get the first registered email
        if (profile.emails && profile.emails.length! > 0) {
          email = profile.emails[0].value;
        }
        const userData = {
          id: profile.id,
          email: email,
          name: profile.displayName,
          picture: profile._json.picture,
        };
        done(null, userData);
      },
    ),
  );
}
