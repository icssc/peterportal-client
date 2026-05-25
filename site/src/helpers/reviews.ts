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

export function getInstructorsForCourseTerm(
  courseProp: CourseGQLData | null | undefined,
  courseId: string,
  yearTaken: string,
  quarterTaken: string,
  professorTermsMap: Record<string, string[]>,
): string[] {
  if (!courseProp || !courseId || !yearTaken || !quarterTaken) {
    return Object.keys(courseProp?.instructors ?? {});
  }

  const selectedTerm = `${yearTaken} ${quarterTaken}`;

  return Object.keys(courseProp.instructors).filter((ucinetid) => {
    const instructorTerms = professorTermsMap[ucinetid] ?? [];
    return instructorTerms.includes(selectedTerm);
  });
}

export function getTermsForInstructor(
  professorGQLData: ProfessorGQLData | null | undefined,
  courseId: string,
): string[] {
  if (!professorGQLData || !professorGQLData.courses[courseId]) {
    return [];
  }
  return professorGQLData.courses[courseId].terms ?? [];
}
