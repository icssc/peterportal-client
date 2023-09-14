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
    professor_history: string[];
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
    name: string;
    shortenedName: string;
    ucinetid: string;
    title: string;
    department: string;
    schools: string[];
    related_departments: string[];
    course_history: string[]
}

export type BatchCourseData = { [key: string]: CourseGQLData };
export type BatchProfessorData = { [key: string]: ProfessorGQLData };

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
    verified: boolean;
}

export interface ReportData {
    _id?: string;
    reviewID: string;
    reason: string;
    timestamp: string;
}

export interface PassportData {
    email: string;
    name: string;
    picture: string;
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

export interface VoteColorRequest {
    id: string;
}

export interface VoteColorsRequest {
    ids: string[];
}

export interface ScoreData {
    name: string;
    score: number;
    key?: string;
}

export type SearchIndex = 'courses' | 'professors';
export type SearchType = 'course' | 'professor';

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


/**
 * Peter's Roadmaps Type Definitions
 */
export type PlannerData = PlannerYearData[];

export interface PlannerYearData {
    startYear: number;
    quarters: PlannerQuarterData[];
}

export interface PlannerQuarterData {
    name: string;
    courses: CourseGQLData[];
}

export type SavedPlannerData = SavedPlannerYearData[];

export interface SavedPlannerYearData {
    startYear: number;
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
};

// Specify name of transfer course and how many units its worth
export interface TransferData {
    name: string;
    units: number | undefined;
}

// Bundle planner and transfer data in one object
export interface SavedRoadmap {
    planner: SavedPlannerData;
    transfers: TransferData[];
}

// Structure stored in mongo for accounts
export interface MongoRoadmap {
    _id: string;
    roadmap: SavedRoadmap;
}

export interface VoteColor {
    colors: boolean[]
}

/**
 * GraphQL Definitions
 */
// Format we deal with
export interface CourseGQLData {
    id: string;
    department: string;
    courseNumber: string;
    school: string;
    title: string;
    courseLevel: string;
    minUnits: number;
    maxUnits: number;
    description: string;
    departmentName: string;
    instructorHistory: ProfessorLookup;
    prerequisiteTree: Record<string, unknown>;
    prerequisiteList: CourseLookup;
    prerequisiteText: string;
    prerequisiteFor: CourseLookup;
    repeatability: string;
    concurrent: string;
    same_as: string;
    restriction: string;
    overlap: string;
    corequisite: string;
    geList: string[];
    gText: string;
    terms: string[];
}

export interface ProfessorGQLData {
    name: string;
    shortenedName: string;
    ucinetid: string;
    title: string;
    department: string;
    schools: string[];
    relatedDepartments: string[];
    courseHistory: CourseLookup;
}

// PPAPI format
export type CourseGQLResponse = Omit<CourseGQLData, 'instructorHistory' | 'prerequisiteList' | 'prerequisiteFor'> & {
    instructorHistory: string[];
    prerequisiteList: string[];
    prerequisiteFor: string[];
}

export type ProfessorGQLResponse = Omit<ProfessorGQLData, 'courseHistory'> & {
    courseHistory: Record<string, string[]>;
}

// maps ucinetid to subprofessor
export interface ProfessorLookup {
    [key: string]: SubProfessor;
}

// maps course id to subcourse
export interface CourseLookup {
    [key: string]: SubCourse;
}

// subset of professor details needed for display purposes
export interface SubProfessor {
    name: string;
    ucinetid: string;
    shortenedName: string;
}

// subset of course details needed for display purposes
export interface SubCourse {
    id: string;
    department: string;
    courseNumber: string;
    title: string;
}
