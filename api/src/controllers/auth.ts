import express, { Request, Response } from 'express';
import { CodeChallengeMethod, generateCodeVerifier, generateState } from 'arctic';
import { db } from '../db';
import { user } from '../db/schema';
import { eq } from 'drizzle-orm';
import { createOIDCClient } from '../config/oidc';

const router = express.Router();

interface OIDCUserInfo {
  sub: string;
  email: string;
  name?: string;
  picture?: string;
}

/**
 * Called after successful authentication
 * Matches user by email and updates/creates user record
 * @param userInfo OIDC user information
 * @param req Express Request Object
 * @param res Express Response Object
 */
async function successLogin(userInfo: OIDCUserInfo, req: Request, res: Response) {
  const { sub, email, name, picture } = userInfo;

  // Match user by email first (for existing users)
  const existingUser = await db.select().from(user).where(eq(user.email, email)).limit(1);

  let userData;
  if (existingUser.length > 0) {
    // Existing user - update their googleId to the new OIDC sub
    userData = await db
      .update(user)
      .set({
        googleId: sub,
        name: name || existingUser[0].name,
        picture: picture || existingUser[0].picture,
      })
      .where(eq(user.email, email))
      .returning();
  } else {
    // New user - create with OIDC sub as googleId
    userData = await db
      .insert(user)
      .values({
        googleId: sub,
        name: name ?? '',
        email,
        picture: picture ?? '',
      })
      .returning();
  }

  req.session.userId = userData[0].id;
  // redirect browser to the page they came from
  const returnTo = req.session.returnTo ?? '/';
  delete req.session.returnTo;
  res.redirect(returnTo!);
}

/**
 * Initiate authentication with OIDC
 */
router.get('/google', async function (req, res) {
  try {
    const oidcClient = createOIDCClient();
    const state = generateState();
    const codeVerifier = generateCodeVerifier();

    req.session.oauthState = state;
    req.session.codeVerifier = codeVerifier;
    req.session.returnTo = req.headers.referer;

    const authUrl = oidcClient.createAuthorizationURLWithPKCE(
      `${process.env.OIDC_ISSUER_URL}/authorize`,
      state,
      CodeChallengeMethod.S256,
      codeVerifier,
      ['openid', 'profile', 'email', 'https://www.googleapis.com/auth/calendar.readonly'],
    );

    res.redirect(authUrl.toString());
  } catch (error) {
    console.error('Error initiating authentication:', error);
    res.redirect('/?error=auth_failed');
  }
});

/**
 * Callback for OIDC authentication
 */
router.get('/google/callback', async function (req, res) {
  const returnTo = req.session.returnTo ?? '/';

  try {
    const code = req.query.code as string;
    const state = req.query.state as string;
    const storedState = req.session.oauthState;
    const codeVerifier = req.session.codeVerifier;

    if (!code || !state || !storedState || state !== storedState || !codeVerifier) {
      console.error('Invalid OAuth state or code');
      res.redirect('/?error=invalid_state');
      return;
    }

    delete req.session.oauthState;
    delete req.session.codeVerifier;

    const oidcClient = createOIDCClient();
    const tokens = await oidcClient.validateAuthorizationCode(
      `${process.env.OIDC_ISSUER_URL}/token`,
      code,
      codeVerifier,
    );

    const userInfoEndpoint = `${process.env.OIDC_ISSUER_URL}/userinfo`;
    const userInfoResponse = await fetch(userInfoEndpoint, {
      headers: {
        Authorization: `Bearer ${tokens.accessToken()}`,
      },
    });

    if (!userInfoResponse.ok) {
      console.error('Failed to fetch user info:', userInfoResponse.statusText);
      res.redirect('/?error=userinfo_failed');
      return;
    }

    const userInfo: OIDCUserInfo = await userInfoResponse.json();

    if (!userInfo.email) {
      console.error('Email not provided by OIDC provider');
      res.redirect('/?error=no_email');
      return;
    }

    req.session.returnTo = returnTo;
    await successLogin(userInfo, req, res);
  } catch (error) {
    console.error('Error in OIDC callback:', error);
    res.redirect('/?error=callback_failed');
  }
});

/**
 * Endpoint to logout
 */
router.get('/logout', function (req, res) {
  req.session.destroy(function (err) {
    if (err) console.error(err);
    // clear the user cookie
    res.clearCookie('user');

    // Redirect to OIDC logout endpoint
    const logoutUrl = new URL(`${process.env.OIDC_ISSUER_URL}/logout`);
    logoutUrl.searchParams.set(
      'post_logout_redirect_uri',
      process.env.PRODUCTION_CLIENT_DOMAIN || 'http://localhost:3000',
    );

    res.redirect(logoutUrl.toString());
  });
});

export default router;
