import path from 'path';
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

passport.use(
    new GoogleStrategy(
        {
            clientID: process.env.GOOGLE_CLIENT,
            clientSecret: process.env.GOOGLE_SECRET,
            callbackURL: (process.env.NODE_ENV == "production" ? process.env.PRODUCTION_DOMAIN : "") + "/users/auth/google/callback"
        },
        function (accessToken, refreshToken, profile, done) {   
            let email = '';
            for (let profileEmail in profile.emails) {
                console.log(profileEmail)
            }         
            var userData = {
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
//     callbackURL: (process.env.NODE_ENV == "development" ? "" : `https://${process.env.DOMAIN}`) + "/users/auth/facebook/callback",
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
//     callbackURL: (process.env.NODE_ENV == "development" ? "" : `https://${process.env.DOMAIN}`) + "/users/auth/github/callback",
//     scope: [ 'user:email', 'user:displayName' ]
//   },
//   function(accessToken, refreshToken, profile, done) {
//     var userData = {
//         email: profile.emails[0].value,
//         name: profile.displayName,
//         picture: profile.photos[0].value,
//         username: profile.username
//     };
//     done(null, userData);
//   }
// ));