/**
 * Express wiring for the MCP server, mounted at `/planner/api/mcp`.
 *
 * Exposes the OAuth 2.1 authorization-server endpoints (authorize/token/register/revoke +
 * discovery metadata), the Google OIDC callback, and the protected MCP endpoint itself
 * (Streamable HTTP, stateless). The OAuth logic lives in {@link mcpOAuthProvider}; the tools
 * live in {@link createMcpServer}.
 *
 * Note on discovery: the SDK's `mcpAuthRouter` assumes the auth server is mounted at the
 * application root and emits root-absolute endpoint paths. Because this app is namespaced
 * under `/planner/api`, we instead build the metadata documents ourselves (with the full
 * `/planner/api/mcp` prefix) and mount the individual handlers. {@link mountMcpWellKnown}
 * additionally serves the discovery documents at the origin root for clients that probe
 * there (works locally; in production those root paths must be routed to this backend).
 * @module
 */

import { authorizationHandler } from '@modelcontextprotocol/sdk/server/auth/handlers/authorize.js';
import { metadataHandler } from '@modelcontextprotocol/sdk/server/auth/handlers/metadata.js';
import { clientRegistrationHandler } from '@modelcontextprotocol/sdk/server/auth/handlers/register.js';
import { revocationHandler } from '@modelcontextprotocol/sdk/server/auth/handlers/revoke.js';
import { tokenHandler } from '@modelcontextprotocol/sdk/server/auth/handlers/token.js';
import { requireBearerAuth } from '@modelcontextprotocol/sdk/server/auth/middleware/bearerAuth.js';
import { StreamableHTTPServerTransport } from '@modelcontextprotocol/sdk/server/streamableHttp.js';
import type { OAuthMetadata, OAuthProtectedResourceMetadata } from '@modelcontextprotocol/sdk/shared/auth.js';
import express, { type Express, type Request, type RequestHandler, type Response } from 'express';
import { getMcpBaseUrl, MCP_DEV_BYPASS_AUTH, MCP_DEV_USER_ID, MCP_SCOPES } from './config';
import { handleMcpGoogleCallback, mcpOAuthProvider } from './provider';
import { createMcpServer } from './server';

/**
 * DEV/TEST ONLY middleware (gated by `MCP_DEV_BYPASS_AUTH`). Skips OAuth and injects a
 * fixed `req.auth` for `MCP_DEV_USER_ID`, so the MCP endpoint can be exercised without a
 * bearer token. See {@link MCP_DEV_BYPASS_AUTH}.
 */
function devAuthBypass(): RequestHandler {
  return (req, _res, next) => {
    req.auth = {
      token: 'dev-bypass',
      clientId: 'dev-bypass',
      scopes: MCP_SCOPES,
      extra: { userId: MCP_DEV_USER_ID },
    };
    next();
  };
}

function buildMetadata(): { oauthMetadata: OAuthMetadata; protectedResourceMetadata: OAuthProtectedResourceMetadata } {
  const base = getMcpBaseUrl();
  const oauthMetadata: OAuthMetadata = {
    issuer: base,
    authorization_endpoint: `${base}/authorize`,
    token_endpoint: `${base}/token`,
    registration_endpoint: `${base}/register`,
    revocation_endpoint: `${base}/revoke`,
    response_types_supported: ['code'],
    grant_types_supported: ['authorization_code', 'refresh_token'],
    code_challenge_methods_supported: ['S256'],
    token_endpoint_auth_methods_supported: ['client_secret_post', 'none'],
    revocation_endpoint_auth_methods_supported: ['client_secret_post', 'none'],
    scopes_supported: MCP_SCOPES,
  };
  const protectedResourceMetadata: OAuthProtectedResourceMetadata = {
    resource: base,
    authorization_servers: [base],
    scopes_supported: MCP_SCOPES,
    resource_name: 'PeterPortal',
  };
  return { oauthMetadata, protectedResourceMetadata };
}

/**
 * Handle a single MCP request. Stateless: a fresh server + transport per request (so it
 * works on Lambda, which can't hold long-lived SSE connections). `enableJsonResponse`
 * returns a single JSON body instead of an SSE stream.
 */
async function handleMcpRequest(req: Request, res: Response): Promise<void> {
  const server = createMcpServer();
  const transport = new StreamableHTTPServerTransport({
    sessionIdGenerator: undefined,
    enableJsonResponse: true,
  });
  res.on('close', () => {
    void transport.close();
    void server.close();
  });
  await server.connect(transport);
  await transport.handleRequest(req, res, req.body);
}

/** Build the router mounted at `/planner/api/mcp`. */
export function createMcpRouter(): express.Router {
  const router = express.Router();
  const { oauthMetadata, protectedResourceMetadata } = buildMetadata();

  // OAuth 2.1 authorization-server endpoints.
  router.use('/authorize', authorizationHandler({ provider: mcpOAuthProvider }));
  router.use('/token', tokenHandler({ provider: mcpOAuthProvider }));
  router.use('/register', clientRegistrationHandler({ clientsStore: mcpOAuthProvider.clientsStore }));
  router.use('/revoke', revocationHandler({ provider: mcpOAuthProvider }));

  // Discovery metadata (also mounted at origin root by mountMcpWellKnown).
  router.use('/.well-known/oauth-authorization-server', metadataHandler(oauthMetadata));
  router.use('/.well-known/oauth-protected-resource', metadataHandler(protectedResourceMetadata));

  // Google OIDC callback that resumes the authorize flow.
  router.get('/auth/google/callback', handleMcpGoogleCallback);

  // Protected MCP endpoint. Normally requires a valid bearer token; the dev bypass
  // (test-only, env-gated) replaces auth with a fixed user so the flow can be exercised
  // without the Google login.
  let authMiddleware: RequestHandler;
  if (MCP_DEV_BYPASS_AUTH) {
    console.warn(
      `⚠️  MCP_DEV_BYPASS_AUTH is ON — the MCP endpoint is UNAUTHENTICATED and every call acts as user ${MCP_DEV_USER_ID}. Never enable this in production.`,
    );
    authMiddleware = devAuthBypass();
  } else {
    authMiddleware = requireBearerAuth({
      verifier: mcpOAuthProvider,
      resourceMetadataUrl: `${getMcpBaseUrl()}/.well-known/oauth-protected-resource`,
    });
  }
  router.post('/', authMiddleware, handleMcpRequest);
  router.get('/', authMiddleware, handleMcpRequest);
  router.delete('/', authMiddleware, handleMcpRequest);

  return router;
}

/**
 * Serve the OAuth discovery documents at the origin root as well, since many MCP clients
 * probe `/.well-known/...` at the host root (RFC 8414 / RFC 9728), optionally with the
 * resource path appended. Mounted on the top-level app, outside the `/planner/api` prefix.
 */
export function mountMcpWellKnown(app: Express): void {
  const { oauthMetadata, protectedResourceMetadata } = buildMetadata();
  const rsPath = new URL(getMcpBaseUrl()).pathname; // e.g. /planner/api/mcp

  app.use('/.well-known/oauth-authorization-server', metadataHandler(oauthMetadata));
  app.use(`/.well-known/oauth-authorization-server${rsPath}`, metadataHandler(oauthMetadata));
  app.use('/.well-known/oauth-protected-resource', metadataHandler(protectedResourceMetadata));
  app.use(`/.well-known/oauth-protected-resource${rsPath}`, metadataHandler(protectedResourceMetadata));
}
