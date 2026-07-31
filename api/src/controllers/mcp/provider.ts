/**
 * OAuth 2.1 authorization-server provider for the MCP server.
 *
 * Implements the MCP SDK's {@link OAuthServerProvider} backed by the `mcp_*` Postgres
 * tables. Identity is delegated to the existing Google OIDC (via `arctic`): `authorize`
 * bounces the browser to Google, and {@link handleMcpGoogleCallback} resumes the flow,
 * enforces the @uci.edu requirement, and issues a one-time authorization code.
 * @module
 */

import { CodeChallengeMethod, generateCodeVerifier } from 'arctic';
import { and, eq, gt } from 'drizzle-orm';
import type { Request, Response } from 'express';
import type { OAuthRegisteredClientsStore } from '@modelcontextprotocol/sdk/server/auth/clients.js';
import type { AuthorizationParams, OAuthServerProvider } from '@modelcontextprotocol/sdk/server/auth/provider.js';
import type { AuthInfo } from '@modelcontextprotocol/sdk/server/auth/types.js';
import type { OAuthClientInformationFull, OAuthTokens } from '@modelcontextprotocol/sdk/shared/auth.js';
import { buildMcpRedirectUri, createOIDCClient } from '../../config/oidc';
import { db } from '../../db';
import { mcpAuthCode, mcpAuthRequest, mcpClient, mcpToken } from '../../db/schema';
import { upsertOidcUser, type OidcUserInfo } from '../../helpers/users';
import {
  MCP_ALLOWED_EMAIL_DOMAIN,
  MCP_AUTH_CODE_TTL_SECONDS,
  MCP_AUTH_REQUEST_TTL_SECONDS,
  MCP_SCOPES,
  MCP_TOKEN_TTL_SECONDS,
  generateSecret,
  hashSecret,
} from './config';

function secondsFromNow(seconds: number): Date {
  return new Date(Date.now() + seconds * 1000);
}

/** Append OAuth response params to a client redirect URI, preserving any existing query. */
function buildRedirect(redirectUri: string, params: Record<string, string>): string {
  const url = new URL(redirectUri);
  for (const [key, value] of Object.entries(params)) url.searchParams.set(key, value);
  return url.toString();
}

const clientsStore: OAuthRegisteredClientsStore = {
  async getClient(clientId) {
    const [row] = await db.select().from(mcpClient).where(eq(mcpClient.clientId, clientId));
    return row ? (row.info as OAuthClientInformationFull) : undefined;
  },
  async registerClient(client) {
    const clientId = generateSecret();
    const info: OAuthClientInformationFull = {
      ...client,
      client_id: clientId,
      client_id_issued_at: Math.floor(Date.now() / 1000),
    };
    await db.insert(mcpClient).values({ clientId, info });
    return info;
  },
};

async function mintTokens(userId: number, clientId: string, scopes: string[]): Promise<OAuthTokens> {
  const accessToken = generateSecret();
  const refreshToken = generateSecret();

  await db.insert(mcpToken).values([
    {
      tokenHash: hashSecret(accessToken),
      type: 'access',
      userId,
      clientId,
      scopes,
      expiresAt: secondsFromNow(MCP_TOKEN_TTL_SECONDS),
    },
    {
      tokenHash: hashSecret(refreshToken),
      type: 'refresh',
      userId,
      clientId,
      scopes,
      expiresAt: null,
    },
  ]);

  return {
    access_token: accessToken,
    token_type: 'bearer',
    expires_in: MCP_TOKEN_TTL_SECONDS,
    refresh_token: refreshToken,
    scope: scopes.join(' '),
  };
}

export const mcpOAuthProvider: OAuthServerProvider = {
  get clientsStore() {
    return clientsStore;
  },

  /**
   * Begin the authorization flow by stashing the request and redirecting to Google.
   * The Google callback ({@link handleMcpGoogleCallback}) finishes issuing the code.
   */
  async authorize(client: OAuthClientInformationFull, params: AuthorizationParams, res: Response) {
    const state = generateSecret();
    const googleCodeVerifier = generateCodeVerifier();
    const scopes = params.scopes?.length ? params.scopes : MCP_SCOPES;

    await db.insert(mcpAuthRequest).values({
      state,
      clientId: client.client_id,
      redirectUri: params.redirectUri,
      clientState: params.state ?? null,
      codeChallenge: params.codeChallenge,
      scopes,
      googleCodeVerifier,
      expiresAt: secondsFromNow(MCP_AUTH_REQUEST_TTL_SECONDS),
    });

    const oidcClient = createOIDCClient(buildMcpRedirectUri());
    const authUrl = oidcClient.createAuthorizationURLWithPKCE(
      `${process.env.OIDC_ISSUER_URL}/authorize`,
      state,
      CodeChallengeMethod.S256,
      googleCodeVerifier,
      ['openid', 'profile', 'email'],
    );
    // We require a UCI Google account: go straight to Google at the broker, and hint the
    // UCI hosted domain. Both are advisory — the @uci.edu check is enforced server-side in
    // the callback.
    authUrl.searchParams.set('provider', 'google');
    authUrl.searchParams.set('hd', 'uci.edu');
    res.redirect(authUrl.toString());
  },

  async challengeForAuthorizationCode(_client: OAuthClientInformationFull, authorizationCode: string): Promise<string> {
    const [row] = await db
      .select({ codeChallenge: mcpAuthCode.codeChallenge })
      .from(mcpAuthCode)
      .where(eq(mcpAuthCode.codeHash, hashSecret(authorizationCode)));
    if (!row) throw new Error('Invalid authorization code');
    return row.codeChallenge;
  },

  async exchangeAuthorizationCode(client: OAuthClientInformationFull, authorizationCode: string): Promise<OAuthTokens> {
    const codeHash = hashSecret(authorizationCode);
    const [row] = await db.select().from(mcpAuthCode).where(eq(mcpAuthCode.codeHash, codeHash));
    if (!row || row.clientId !== client.client_id || row.expiresAt.getTime() < Date.now()) {
      throw new Error('Invalid or expired authorization code');
    }
    // Authorization codes are single-use.
    await db.delete(mcpAuthCode).where(eq(mcpAuthCode.codeHash, codeHash));
    return mintTokens(row.userId, row.clientId, row.scopes);
  },

  async exchangeRefreshToken(
    client: OAuthClientInformationFull,
    refreshToken: string,
    scopes?: string[],
  ): Promise<OAuthTokens> {
    const tokenHash = hashSecret(refreshToken);
    const [row] = await db
      .select()
      .from(mcpToken)
      .where(and(eq(mcpToken.tokenHash, tokenHash), eq(mcpToken.type, 'refresh')));
    if (!row || row.clientId !== client.client_id) throw new Error('Invalid refresh token');
    // Requested scopes (if any) must be a subset of the originally granted scopes.
    const granted = scopes?.length ? scopes.filter((s) => row.scopes.includes(s)) : row.scopes;
    return mintTokens(row.userId, row.clientId, granted);
  },

  async verifyAccessToken(token: string): Promise<AuthInfo> {
    const [row] = await db
      .select()
      .from(mcpToken)
      .where(and(eq(mcpToken.tokenHash, hashSecret(token)), eq(mcpToken.type, 'access')));
    if (!row) throw new Error('Invalid access token');
    if (row.expiresAt && row.expiresAt.getTime() < Date.now()) throw new Error('Access token expired');
    return {
      token,
      clientId: row.clientId,
      scopes: row.scopes,
      expiresAt: row.expiresAt ? Math.floor(row.expiresAt.getTime() / 1000) : undefined,
      // The authenticated student's internal user id, read by tool handlers.
      extra: { userId: row.userId },
    };
  },

  async revokeToken(_client: OAuthClientInformationFull, request: { token: string }): Promise<void> {
    await db.delete(mcpToken).where(eq(mcpToken.tokenHash, hashSecret(request.token)));
  },
};

/**
 * Resolve the internal user id carried by a verified MCP access token.
 * Returns `undefined` if the auth info is missing or malformed.
 */
export function userIdFromAuthInfo(auth: AuthInfo | undefined): number | undefined {
  const userId = auth?.extra?.userId;
  return typeof userId === 'number' ? userId : undefined;
}

/**
 * Express handler for the Google OIDC callback in the MCP flow.
 *
 * Resumes the authorization request identified by `state`, exchanges the Google code,
 * enforces the @uci.edu requirement, upserts the user, and redirects back to the MCP
 * client with a one-time authorization code (or an OAuth error).
 */
export async function handleMcpGoogleCallback(req: Request, res: Response): Promise<void> {
  const code = req.query.code as string | undefined;
  const state = req.query.state as string | undefined;

  if (!code || !state) {
    res.status(400).send('Missing code or state');
    return;
  }

  const [authRequest] = await db
    .select()
    .from(mcpAuthRequest)
    .where(and(eq(mcpAuthRequest.state, state), gt(mcpAuthRequest.expiresAt, new Date())));

  if (!authRequest) {
    res.status(400).send('Invalid or expired authorization request');
    return;
  }
  await db.delete(mcpAuthRequest).where(eq(mcpAuthRequest.state, state));

  const clientStateParam: Record<string, string> = authRequest.clientState ? { state: authRequest.clientState } : {};

  try {
    const oidcClient = createOIDCClient(buildMcpRedirectUri());
    const tokens = await oidcClient.validateAuthorizationCode(
      `${process.env.OIDC_ISSUER_URL}/token`,
      code,
      authRequest.googleCodeVerifier,
    );

    const userInfoResponse = await fetch(`${process.env.OIDC_ISSUER_URL}/userinfo`, {
      headers: { Authorization: `Bearer ${tokens.accessToken()}` },
    });
    if (!userInfoResponse.ok) {
      res.redirect(buildRedirect(authRequest.redirectUri, { error: 'server_error', ...clientStateParam }));
      return;
    }

    const userInfo: OidcUserInfo = await userInfoResponse.json();

    if (!userInfo.email || !userInfo.email.toLowerCase().endsWith(MCP_ALLOWED_EMAIL_DOMAIN)) {
      res.redirect(
        buildRedirect(authRequest.redirectUri, {
          error: 'access_denied',
          error_description: `Sign in with a ${MCP_ALLOWED_EMAIL_DOMAIN} Google account to use PeterPortal MCP.`,
          ...clientStateParam,
        }),
      );
      return;
    }

    const dbUser = await upsertOidcUser(userInfo);

    const authorizationCode = generateSecret();
    await db.insert(mcpAuthCode).values({
      codeHash: hashSecret(authorizationCode),
      userId: dbUser.id,
      clientId: authRequest.clientId,
      redirectUri: authRequest.redirectUri,
      codeChallenge: authRequest.codeChallenge,
      scopes: authRequest.scopes,
      expiresAt: secondsFromNow(MCP_AUTH_CODE_TTL_SECONDS),
    });

    res.redirect(buildRedirect(authRequest.redirectUri, { code: authorizationCode, ...clientStateParam }));
  } catch (error) {
    console.error('MCP Google callback error:', error);
    res.redirect(buildRedirect(authRequest.redirectUri, { error: 'server_error', ...clientStateParam }));
  }
}
