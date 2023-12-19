import { integer } from "aws-sdk/clients/cloudfront";

export interface GenericObject {
    /**
     * Can have any key string and any value type
     */
    [key: string]: any;
}

export interface QuarterMapping {
    /**
     * Maps quarter name to a date range
     */
    [key: string]: DateRange;
}

export interface DateRange {
    /**
     * Beginning date of the range
     */
    begin: Date;
    /**
     * Ending date of the range
     */
    end: Date;
}

export interface WeekData {
    /**
     * The week number of the quarter.
     * During quarter: 1-10
     * Finals: 11
     * Break: 0
     */
    week: number;
    /**
     * The current quarter
     */
    quarter: string;
    /**
     * The display friendly string. Special case for finals and break.
     */
    display: string;
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


declare module "express-session" {
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
