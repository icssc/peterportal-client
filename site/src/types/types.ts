export interface CourseData {
    id: string;
    department: string;
    number: string;
    school: string;
    title: string;
    course_level: string;
    department_alias: string[];
    units: number[];
    description: string;
    department_name: string;
    professor_history: string;
    prerequisite_tree: string;
    prerequisite_list: string[];
    prerequisite_text: string;
    prerequisite_for: string[];
    repeatability: string;
    grading_option: string;
    concurrent: string;
    same_as: string;
    restriction: string;
    overlap: string;
    corequisite: string;
    ge_list: string[];
    ge_text: string;
    terms: string[]
}

export interface ProfessorData {
    name: string,
    ucinetid: string,
    title: string,
    department: string,
    schools: string[],
    related_departments: string[],
    course_history: string[]
}

export type GradeDistData = GradeData[];

export interface GradeData {
  year: string;
  quarter: string;
  department: string;
  number: string;
  code: number;
  section: string;
  instructor: string;
  type: string;
  gradeACount: number;
  gradeBCount: number;
  gradeCCount: number;
  gradeDCount: number;
  gradeFCount: number;
  gradePCount: number;
  gradeNPCount: number;
  gradeWCount: number;
  averageGPA: number;
}

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
}

export interface PassportData {
    email: string,
    name: string,
    picture: string,
    username?: string
}

export interface PrerequisiteJSON {
    [key: string]: PrerequisiteJSONNode[];
}

export type PrerequisiteJSONNode = PrerequisiteJSON | string;

export interface VoteRequest {
    id: string;
    upvote: boolean;
}

export type ElasticSearchIndex = 'courses' | 'professors'

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

export interface GenericObject {
    /**
     * Can have any key string and any value type
     */
    [key: string]: any;
}