import { ReviewData } from '@peterportal/types';
import { CourseGQLData, ProfessorGQLData } from '../types/types';

export function getProfessorTerms(professorGQLData: ProfessorGQLData): string[] {
  return Object.values(professorGQLData.courses).flatMap((c) => c.terms);
}

export function getYears(terms: string[]): string[] {
  return [...new Set(terms.map((t) => t.split(' ')[0]))];
}

export function getQuarters(terms: string[], yearTaken: string): string[] {
  return [...new Set(terms.filter((t) => t.startsWith(yearTaken)).map((t) => t.split(' ')[1]))];
}

export function getReviewHeadingName(
  reviewToEdit: ReviewData | undefined,
  course: CourseGQLData | undefined,
  instructor: ProfessorGQLData | undefined,
) {
  if (!course && !instructor) {
    return `${reviewToEdit?.courseId}`;
  } else if (course) {
    return `${course?.department} ${course?.courseNumber}`;
  } else {
    return `${instructor?.name}`;
  }
}
