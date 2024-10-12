import { z } from 'zod';

export const reportSubmission = z.object({
  reviewID: z.string(),
  reason: z.string().min(1).max(500),
});
export type ReportSubmission = z.infer<typeof reportSubmission>;

export const reportData = reportSubmission.extend({
  _id: z.string(),
  timestamp: z.string(),
});
export type ReportData = z.infer<typeof reportData>;

export const theme = z.enum(['light', 'dark', 'system']);
export type Theme = z.infer<typeof theme>;

export const userPreferences = z.object({
  theme: theme,
});
export type UserPreferences = z.infer<typeof userPreferences>;

export interface ProfessorPreview {
  name: string;
  ucinetid: string;
  shortenedName: string;
}

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
  // TODO: can we feasibly type this?
  prerequisiteTree: Record<string, unknown>;
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
  courseHistory: Record<string, string[]>;
}

export type ProfessorBatchAAPIResponse = Record<string, ProfessorAAPIResponse>;

// TODO: should make use of this on frontend when accessing user cookie with useCookies
export interface UserData {
  id: string;
  email: string;
  name: string;
  picture: string;
}

export * from 'peterportal-api-next-types';
