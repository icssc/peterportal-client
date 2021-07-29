export interface GenericObject {
    [key: string]: any;
}

export interface QuarterMapping {
    [key: string]: DateRange
}

export interface DateRange {
    begin: Date;
    end: Date;
}

export interface WeekData {
    week: number;
    quarter: string; 
    display: string;
}

import session from 'express-session';
declare module 'express-session' {
  export interface SessionData {
    passport: PassportData;
    returnTo: string;
  }

  interface PassportData {
      admin: boolean
  }
}