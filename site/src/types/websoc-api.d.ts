declare module "websoc-api" {
    export interface WebsocResponse {
        schools: School[]
    } 
    export interface School {
        schoolName: string;
        schoolComment: string;
        departments: Department[];
    }
    export interface Department {
        deptName: string;
        deptCode: string;
        deptComment: string;
        courses: Course[];
        sectionCodeRangeComments: string[];
        courseNumberRangeComments: string[];
    }
    export interface Course {
        courseNumber: string;
        courseTitle: string;
        courseComment: string;
        prerequisiteLink: string;
        sections: Section[];
    }
    export interface Section {
        sectionCode: string;
        sectionType: string;
        sectionNum: string;
        units: string;
        instructors: string[];
        meetings: Meeting[];
        finalExam: string;
        maxCapacity: string;
        numCurrentlyEnrolled: EnrollmentCount;
        numOnWaitlist: string;
        numRequested: string;
        numNewOnlyReserved: string;
        restrictions: string;
        status: string;
        sectionComment: string;
    }
    export interface Meeting {
        days: string;
        time: string;
        bldg: string;
    }
    export interface EnrollmentCount {
        totalEnrolled: string;
        sectionEnrolled: string;
    }
}