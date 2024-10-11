import 'express-session';

declare module 'express-session' {
  export interface SessionData {
    /**
     * Store custom data in passport
     */
    passport: PassportData;
    /**
     * URL to return to when finish authentication
     */
    returnTo: string;
  }

  export interface PassportData {
    /**
     * True if is validated as an admin
     */
    admin: boolean;
    user: User;
  }

  interface User {
    id: string;
    email: string;
    name: string;
    picture: string;
  }
}

export {};
