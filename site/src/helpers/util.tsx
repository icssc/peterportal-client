import { CourseData, ProfessorData } from '../types/types';

export function getCourseTags(course: CourseData) {
  // data to be displayed in pills
  let tags: string[] = [];
  // course level
  let courseLevel = course.course_level;
  if (courseLevel) {
    tags.push(`${courseLevel.substring(0, courseLevel.indexOf('('))}`);
  }
  // ge
  course.ge_list.forEach(ge => {
    tags.push(`${ge.substring(0, ge.indexOf(':'))}`);
  })
  // units
  let units = course.units[0]
  tags.push(`${units} unit${units != 1 ? 's' : ''}`);
  return tags;
}
