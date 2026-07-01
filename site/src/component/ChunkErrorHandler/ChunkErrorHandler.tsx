'use client';

import { useEffect } from 'react';

export default function ChunkErrorHandler() {
  useEffect(() => {
    const handleError = (event: ErrorEvent) => {
      if (event.message.includes('ChunkLoadError') || event.message.includes('Failed to load chunk')) {
        console.warn('ChunkLoadError detected, reloading page...', event);
        window.location.reload();
      }
    };

    const handleUnhandledRejection = (event: PromiseRejectionEvent) => {
      if (
        event.reason?.message?.includes('ChunkLoadError') ||
        event.reason?.message?.includes('Failed to load chunk')
      ) {
        console.warn('ChunkLoadError in unhandled rejection, reloading page...', event);
        window.location.reload();
      }
    };

    window.addEventListener('error', handleError);
    window.addEventListener('unhandledrejection', handleUnhandledRejection);

    return () => {
      window.removeEventListener('error', handleError);
      window.removeEventListener('unhandledrejection', handleUnhandledRejection);
    };
  }, []);

  return null;
}
