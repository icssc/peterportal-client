import { Quarter } from 'peterportal-api-next-types';
import { CoursePreview } from './course';

export interface ProfessorPreview {
  name: string;
  ucinetid: string;
  shortenedName: string;
}

export type YearAndQuarter = `${number} ${Quarter}`;

export interface ProfessorAAPIResponse {
  name: string;
  shortenedName: string;
  ucinetid: string;
  title: string;
  department: string;
  schools: string[];
  relatedDepartments: string[];
  courses: CoursePreview[];
  /**
   * keys are course names (with spaces e.g. I&C SCI 46), values are an array of terms (e.g. ["2020 Fall", "2020 Summer10wk"])
   */
  courseHistory: Record<string, YearAndQuarter[]>;
}

export type ProfessorBatchAAPIResponse = Record<string, ProfessorAAPIResponse>;
