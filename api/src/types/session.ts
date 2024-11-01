import { PassportUser } from '@peterportal/types';
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
    userId: number;
    isAdmin: boolean;
  }

  export interface PassportData {
    user: PassportUser;
  }
}
