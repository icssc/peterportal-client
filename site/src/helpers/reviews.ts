import { ProfessorGQLData } from '../types/types';

export function getProfessorTerms(professorGQLData: ProfessorGQLData): string[] {
  return Object.values(professorGQLData.courses).flatMap((c) => c.terms);
}

export function getYears(terms: string[]): string[] {
  return [...new Set(terms.map((t) => t.split(' ')[0]))];
}

export function getQuarters(terms: string[], yearTaken: string): string[] {
  return [...new Set(terms.filter((t) => t.startsWith(yearTaken)).map((t) => t.split(' ')[1]))];
}

/* 
Get available terms for a specific instructor teaching a specific course
*/
export function getTermsForInstructor(
  professorGQLData: ProfessorGQLData | null | undefined,
  courseId: string,
): string[] {
  if (!professorGQLData || !professorGQLData.courses[courseId]) {
    return [];
  }
  return professorGQLData.courses[courseId].terms ?? [];
}
