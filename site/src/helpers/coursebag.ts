import { CourseGQLData } from '../types/types';

function compareCourses(a: CourseGQLData, b: CourseGQLData): number {
  return (
    a.department.localeCompare(b.department) ||
    a.courseNumeric - b.courseNumeric ||
    a.courseNumber.localeCompare(b.courseNumber)
  );
}

export function sortCoursebag(coursebag: CourseGQLData[]): CourseGQLData[] {
  return coursebag.sort(compareCourses);
}

export function getCoursebagSortedIndex(coursebag: CourseGQLData[], newCourse: CourseGQLData): number {
  return coursebag.findIndex((c) => compareCourses(c, newCourse) > 0);
}
