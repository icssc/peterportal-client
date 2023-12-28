import axios from 'axios';
import {
  SearchIndex,
  CourseGQLData,
  CourseGQLResponse,
  ProfessorGQLData,
  ProfessorGQLResponse,
  ProfessorLookup,
  CourseLookup,
  BatchCourseData,
  BatchProfessorData,
  SearchType,
} from '../types/types';

export function getCourseTags(course: CourseGQLData) {
  // data to be displayed in pills
  let tags: string[] = [];
  // course level
  let courseLevel = course.courseLevel;
  if (courseLevel) {
    tags.push(`${courseLevel.substring(0, courseLevel.indexOf('('))}`);
  }
  // ge
  course.geList.forEach((ge) => {
    tags.push(`${ge.substring(0, ge.indexOf(':'))}`);
  });
  // units
  const { minUnits, maxUnits } = course;
  tags.push(
    `${minUnits === maxUnits ? maxUnits : `${minUnits}-${maxUnits}`} unit${
      minUnits === maxUnits ? (maxUnits !== 1 ? 's' : '') : 's'
    }`,
  );
  return tags;
}

// helper function to search 1 result from course/professor page
export function searchAPIResult(type: SearchType, name: string) {
  return new Promise<CourseGQLData | ProfessorGQLData | undefined>((res) => {
    let index: SearchIndex;
    if (type === 'course') {
      index = 'courses';
    } else {
      index = 'professors';
    }
    searchAPIResults(index, [name]).then((results) => {
      if (Object.keys(results).length > 0) {
        let key = Object.keys(results)[0];
        res(results[key]);
      } else {
        res(undefined);
      }
    });
  });
}

// helper function to query from API and transform to data used in redux
export async function searchAPIResults(
  index: SearchIndex,
  names: string[],
): Promise<BatchCourseData | BatchProfessorData> {
  const res = await axios.post<{ [key: string]: CourseGQLResponse | ProfessorGQLResponse }>(`/api/${index}/api/batch`, {
    [index]: names,
  });
  const data = res.data;
  const transformed: BatchCourseData | BatchProfessorData = {};
  for (const id in data) {
    if (data[id]) {
      // use specific key based on index
      let key = '';
      if (index == 'courses') {
        key = (data[id] as CourseGQLResponse).id;
      } else {
        key = (data[id] as ProfessorGQLResponse).ucinetid;
      }
      // perform transformation
      transformed[key] = data[id] as unknown as CourseGQLData | ProfessorGQLData;
    }
  }
  console.log('From backend search', transformed);
  return transformed;
}

export const hourMinuteTo12HourString = ({ hour, minute }: { hour: number; minute: number }) =>
  `${hour === 12 ? 12 : hour % 12}:${minute.toString().padStart(2, '0')} ${Math.floor(hour / 12) === 0 ? 'AM' : 'PM'}`;
