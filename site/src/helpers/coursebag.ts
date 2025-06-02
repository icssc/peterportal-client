import { CourseGQLData } from '../types/types';

export function sortCoursebag(coursebag: CourseGQLData[]): CourseGQLData[] {
  return coursebag.sort((a, b) => a.department.localeCompare(b.department) || a.courseNumeric - b.courseNumeric);
}
