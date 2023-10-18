/**
 @module PassportConfig
*/

import passport from 'passport';
import { OAuth2Strategy as GoogleStrategy, VerifyOptions } from 'passport-google-oauth'
// var FacebookStrategy = require('passport-facebook').Strategy;
// var GitHubStrategy = require('passport-github').Strategy;

passport.serializeUser(function (user, done) {
    done(null, user);
});

passport.deserializeUser(function (user: any, done) {
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
            var userData = {
                id: profile.id,
                email: email,
                name: profile.displayName,
                picture: profile._json.picture
            };
            done(null, userData);
        }
    )
);

// passport.use(new FacebookStrategy({
//     clientID: process.env.FACEBOOK_CLIENT,
//     clientSecret: process.env.FACEBOOK_SECRET,
//     callbackURL: (process.env.NODE_ENV == 'development' ? '' : `https://${process.env.DOMAIN}`) + '/api/users/auth/facebook/callback',
//     profileFields: ['id', 'emails', 'displayName', 'photos']
//   },
//   function(accessToken, refreshToken, profile, done) {
//     var userData = {
//         email: profile.emails[0].value,
//         name: profile.displayName,
//         picture: profile.photos[0].value
//     };
//     done(null, userData);
//   }
// ));

// passport.use(new GitHubStrategy({
//     clientID: process.env.GITHUB_CLIENT,
//     clientSecret: process.env.GITHUB_SECRET,
//     callbackURL: (process.env.NODE_ENV == 'development' ? '' : `https://${process.env.DOMAIN}`) + '/api/users/auth/github/callback',
//     scope: [ 'user:email', 'user:displayName' ]
//   },
//   function(accessToken, refreshToken, profile, done) {
//     var userData = {
//         email: profile.emails[0].value,
//         name: profile.displayName,
//         picture: profile.photos[0].value
//     };
//     done(null, userData);
//   }
// ));
