declare global {
  namespace NodeJS {
    /**
     * Define schema for environment variables
     */
    interface ProcessEnv {
      NODE_ENV?: 'development' | 'production' | 'staging';
      PORT?: string;
      PUBLIC_API_URL: string;
      PUBLIC_API_GRAPHQL_URL: string;
      MONGO_URL: string;
      SESSION_SECRET: string;
      GOOGLE_CLIENT: string;
      GOOGLE_SECRET: string;
      GRECAPTCHA_SECRET: string;
      PRODUCTION_DOMAIN: string;
      ADMIN_EMAILS: string;
    }
  }
}

// need to export something to be considered a 'module'
export {};
