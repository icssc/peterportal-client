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
