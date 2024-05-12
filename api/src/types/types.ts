export interface GenericObject {
  /**
   * Can have any key string and any value type
   */
  [key: string]: unknown;
}

export interface VoteData {
  _id?: string;
  userID: string;
  reviewID: string;
  score: number;
}

/** @todo should have some shared types between server and client */
export interface ReviewData {
  _id?: string;
  professorID: string;
  courseID: string;
  userID: string;
  userDisplay: string;
  reviewContent: string;
  rating: number;
  difficulty: number;
  timestamp: string;
  gradeReceived: string;
  forCredit: boolean;
  quarter: string;
  score: number;
  takeAgain: boolean;
  textbook: boolean;
  attendance: boolean;
  tags: string[];
  verified?: boolean;
  captchaToken: string;
}

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

  interface PassportData {
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
