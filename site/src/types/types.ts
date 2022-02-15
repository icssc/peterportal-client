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
    shortened_name: string;
    ucinetid: string;
    title: string;
    department: string;
    schools: string[];
    related_departments: string[];
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
    takeAgain: boolean;
    textbook: boolean;
    attendance: boolean;
    tags: string[];
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

export interface ScoreData {
    name: string;
    score: number;
}

export type ElasticSearchIndex = 'courses' | 'professors';
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
    courses: CourseData[];
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

/**
 * GraphQL Definitions
 */
export interface CourseGQLData {
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
    instructor_history: ProfessorLookup;
    prerequisite_tree: string;
    prerequisite_list: CourseLookup;
    prerequisite_text: string;
    prerequisite_for: CourseLookup;
    repeatability: string;
    concurrent: string;
    same_as: string;
    restriction: string;
    overlap: string;
    corequisite: string;
    ge_list: string[];
    ge_text: string;
    terms: string[];
}

export interface ProfessorGQLData {
    name: string;
    shortened_name: string;
    ucinetid: string;
    title: string;
    department: string;
    schools: string[];
    related_departments: string[];
    course_history: CourseLookup;
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
    shortened_name: string;
}

// subset of course details needed for display purposes
export interface SubCourse {
    id: string;
    department: string;
    number: string;
    title: string;
}