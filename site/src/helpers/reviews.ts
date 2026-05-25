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

export function formatQuarter(quarter: string): string {
  const [year, term] = quarter.split(' ');
  const termMap: Record<string, string> = {
    Fall: 'F',
    Winter: 'W',
    Spring: 'Sp',
    Summer: 'Su',
  };
  return `${termMap[term] ?? term}${year.slice(-2)}`;
}

export function displayReviewDate(date: string | Date): string {
  return new Date(date).toLocaleString('default', {
    year: 'numeric',
    month: '2-digit',
    day: '2-digit',
  });
}
