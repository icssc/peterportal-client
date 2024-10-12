import { UserData } from 'aws-cdk-lib/aws-ec2';
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
    user: UserData;
  }
}

export {};
