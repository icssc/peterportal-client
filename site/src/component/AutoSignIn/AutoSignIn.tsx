'use client';

import { useEffect, useRef } from 'react';
import { useIsLoggedIn } from '../../hooks/isLoggedIn';

const AUTH_ORIGIN = 'https://auth.icssc.club';
const SESSION_CHECK_TIMEOUT = 5000;

interface SessionCheckResult {
  valid: boolean;
  user: {
    id: string;
    email: string;
    name: string;
    picture?: string;
  } | null;
}

/**
 * Automatically signs in users who have an existing ICSSC session.
 * This enables single sign-on across ICSSC apps (e.g., AntAlmanac, PeterPortal).
 *
 * How it works:
 * 1. Loads a hidden iframe pointing to auth.icssc.club/session/check
 * 2. The iframe uses postMessage to communicate if the user has an active session
 * 3. If a session exists, redirects to the OAuth sign-in flow (which completes instantly)
 */
export function AutoSignIn() {
  const isLoggedIn = useIsLoggedIn();
  const hasChecked = useRef(false);

  useEffect(() => {
    // Skip if already checked, or user is already logged in
    if (hasChecked.current || isLoggedIn) {
      return;
    }
    hasChecked.current = true;

    checkIcsscSession().then((result) => {
      if (result.valid) {
        // Redirect to PeterPortal's OIDC sign-in endpoint
        // The OAuth flow will complete instantly since the ICSSC session already exists
        window.location.href = '/planner/api/users/auth/google';
      }
    });
  }, [isLoggedIn]);

  return null;
}

/**
 * Checks for an existing ICSSC session using a hidden iframe.
 * The auth server's /session/check endpoint responds via postMessage.
 */
function checkIcsscSession(): Promise<SessionCheckResult> {
  return new Promise((resolve) => {
    const timeout = setTimeout(() => {
      cleanup();
      resolve({ valid: false, user: null });
    }, SESSION_CHECK_TIMEOUT);

    const iframe = document.createElement('iframe');
    iframe.style.display = 'none';
    iframe.src = `${AUTH_ORIGIN}/session/check?origin=${encodeURIComponent(window.location.origin)}`;

    const handleMessage = (event: MessageEvent) => {
      if (event.origin !== AUTH_ORIGIN) return;
      if (event.data?.type !== 'icssc-session-check') return;

      cleanup();
      resolve({
        valid: event.data.valid,
        user: event.data.user,
      });
    };

    const cleanup = () => {
      clearTimeout(timeout);
      window.removeEventListener('message', handleMessage);
      if (iframe.parentNode) {
        iframe.parentNode.removeChild(iframe);
      }
    };

    window.addEventListener('message', handleMessage);
    document.body.appendChild(iframe);
  });
}

export default AutoSignIn;
