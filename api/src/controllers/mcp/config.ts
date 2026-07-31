/**
 * Shared configuration and crypto helpers for the MCP server.
 * @module
 */

import { createHash, randomBytes } from 'crypto';

/** OAuth scope advertised + issued. Plan access is read-only. */
export const MCP_SCOPES = ['mcp:read'];

/** Access-token lifetime in seconds (default 1h). */
export const MCP_TOKEN_TTL_SECONDS = Number(process.env.MCP_TOKEN_TTL_SECONDS ?? 3600);

/** Lifetime of an in-flight `/authorize` request before the Google round-trip must finish. */
export const MCP_AUTH_REQUEST_TTL_SECONDS = 600;

/** Lifetime of a one-time authorization code. */
export const MCP_AUTH_CODE_TTL_SECONDS = 60;

/** Per-user daily tool-call cap. Configurable for testing. */
export const MCP_DAILY_REQUEST_LIMIT = Number(process.env.MCP_DAILY_REQUEST_LIMIT ?? 200);

/** Domain students must authenticate with. */
export const MCP_ALLOWED_EMAIL_DOMAIN = '@uci.edu';

/**
 * DEV/TEST ONLY. When `MCP_DEV_BYPASS_AUTH=true`, the MCP endpoint skips OAuth entirely
 * and every request acts as `MCP_DEV_USER_ID`. This exists solely so the server can be
 * exercised (e.g. through a tunnel into Claude) without completing the Google login, which
 * the dev identity provider only allows for localhost redirects. NEVER enable in production.
 */
export const MCP_DEV_BYPASS_AUTH = process.env.MCP_DEV_BYPASS_AUTH === 'true';
export const MCP_DEV_USER_ID = process.env.MCP_DEV_USER_ID ? Number(process.env.MCP_DEV_USER_ID) : undefined;

/**
 * Absolute base URL of the MCP server, e.g. `https://antalmanac.com/planner/api/mcp`.
 * Used as the OAuth issuer, the resource identifier, and the base for endpoint metadata.
 * Falls back to localhost for local dev (the SDK permits an http issuer on localhost).
 */
export function getMcpBaseUrl(): string {
  const domain = process.env.PRODUCTION_DOMAIN ?? 'http://localhost:8080';
  return `${domain.replace(/\/$/, '')}/planner/api/mcp`;
}

/** Opaque random secret used for tokens / codes / state, URL-safe. */
export function generateSecret(): string {
  return randomBytes(32).toString('base64url');
}

/** SHA-256 hex digest. Codes and tokens are persisted only as hashes. */
export function hashSecret(secret: string): string {
  return createHash('sha256').update(secret).digest('hex');
}
