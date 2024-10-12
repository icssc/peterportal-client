import { CourseAAPIResponse, CoursePreview, ProfessorAAPIResponse, ProfessorPreview } from '@peterportal/types';

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
  captchaToken?: string;
  userVote?: number;
}

export type FeaturedReviewData = Omit<ReviewData, 'userVote'>;

export interface PassportData {
  email: string;
  name: string;
  picture: string;
  username?: string;
}

export interface PrerequisiteJSON {
  [key: string]: PrerequisiteJSONNode[];
}

export type PrerequisiteJSONNode = PrerequisiteJSON | string;

export interface VoteRequest {
  id: string;
  upvote: boolean;
}

export interface ScoreData {
  name: string;
  score: number;
  key?: string;
}

export type SearchIndex = 'courses' | 'professors';
export type SearchType = 'course' | 'professor';

/**
 * Peter's Roadmaps Type Definitions
 */
export type PlannerData = PlannerYearData[];

export interface PlannerYearData {
  startYear: number;
  name: string;
  quarters: PlannerQuarterData[];
}

export interface PlannerQuarterData {
  name: QuarterName;
  courses: CourseGQLData[];
}

export type SavedPlannerData = {
  name: string;
  content: SavedPlannerYearData[];
};

export interface SavedPlannerYearData {
  startYear: number;
  name: string;
  quarters: SavedPlannerQuarterData[];
}

export interface SavedPlannerQuarterData {
  name: string;
  courses: string[];
}

// Specify the location of a year
export interface YearIdentifier {
  yearIndex: number;
}

export type QuarterName = 'Fall' | 'Winter' | 'Spring' | 'Summer1' | 'Summer2' | 'Summer10wk';

// Specify the location of a quarter
export interface QuarterIdentifier extends YearIdentifier {
  quarterIndex: number;
}

// Specify the location of a course
export interface CourseIdentifier extends QuarterIdentifier {
  courseIndex: number;
}

// Specify where the invalid course is and what courses it needs to take
export interface InvalidCourseData {
  location: CourseIdentifier;
  required: string[];
}

// Specify name of transfer course and how many units its worth
export interface TransferData {
  name: string;
  units: number | undefined;
}

// Bundle planner and transfer data in one object
export interface SavedRoadmap {
  timestamp: number;
  planners: SavedPlannerData[];
  transfers: TransferData[];
}

// Structure stored in mongo for accounts
export interface MongoRoadmap {
  _id: string;
  roadmap: SavedRoadmap;
}

export interface ProfessorLookup {
  [ucinetid: string]: ProfessorPreview;
}

export interface CourseLookup {
  [courseid: string]: CoursePreview;
}

export type CourseGQLData = Omit<CourseAAPIResponse, 'instructors' | 'prerequisites' | 'dependencies'> & {
  instructors: ProfessorLookup;
  prerequisites: CourseLookup;
  dependencies: CourseLookup;
};

export interface BatchCourseData {
  [courseid: string]: CourseGQLData;
}

export type ProfessorGQLData = Omit<ProfessorAAPIResponse, 'courses'> & {
  courses: CourseLookup;
};

export interface BatchProfessorData {
  [ucinetid: string]: ProfessorGQLData;
}
