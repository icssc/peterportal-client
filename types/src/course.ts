import { Prerequisite, PrerequisiteTree } from 'peterportal-api-next-types';
import { ProfessorPreview } from './professor';

export interface CoursePreview {
  id: string;
  department: string;
  courseNumber: string;
  title: string;
}

export interface CourseAAPIResponse {
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
  instructors: ProfessorPreview[];
  prerequisiteTree: Record<string, Prerequisite | PrerequisiteTree>;
  prerequisites: CoursePreview[];
  prerequisiteText: string;
  dependencies: CoursePreview[];
  repeatability: string;
  concurrent: string;
  sameAs: string;
  restriction: string;
  overlap: string;
  corequisites: string;
  geList: string[];
  geText: string;
  terms: string[];
}

export type CourseBatchAAPIResponse = Record<string, CourseAAPIResponse>;
