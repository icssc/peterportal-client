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
    [key: string]: DateRange
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
        admin: boolean
    }
}