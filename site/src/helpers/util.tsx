import {
  SearchIndex,
  CourseGQLData,
  ProfessorGQLData,
  ProfessorLookup,
  CourseLookup,
  BatchCourseData,
  BatchProfessorData,
  SearchType,
} from '../types/types';
import { useMediaQuery } from 'react-responsive';
import trpc from '../trpc';
import { CourseAAPIResponse, ProfessorAAPIResponse } from '@peterportal/types';

export function getCourseTags(course: CourseGQLData) {
  // data to be displayed in pills
  const tags: string[] = [];
  // units
  const { minUnits, maxUnits } = course;
  tags.push(
    `${minUnits === maxUnits ? maxUnits : `${minUnits}-${maxUnits}`} unit${
      minUnits === maxUnits ? (maxUnits !== 1 ? 's' : '') : 's'
    }`,
  );
  // course level
  const courseLevel = course.courseLevel;
  if (courseLevel) {
    tags.push(`${courseLevel.substring(0, courseLevel.indexOf('('))}`);
  }
  // ge
  course.geList.forEach((ge) => {
    tags.push(`${ge.substring(0, ge.indexOf(':'))}`);
  });
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
        const key = Object.keys(results)[0];
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
  const data =
    index === 'courses'
      ? await trpc.courses.batch.mutate({ courses: names })
      : await trpc.professors.batch.mutate({ professors: names });

  const transformed: BatchCourseData | BatchProfessorData = {};
  for (const id in data) {
    if (data[id]) {
      // use specific key based on index
      let key = '';
      if (index == 'courses') {
        key = (data[id] as CourseAAPIResponse).id;
      } else {
        key = (data[id] as ProfessorAAPIResponse).ucinetid;
      }
      // perform transformation
      transformed[key] = transformGQLData(index, data[id]);
    }
  }
  return transformed;
}

export const hourMinuteTo12HourString = ({ hour, minute }: { hour: number; minute: number }) =>
  `${hour === 12 ? 12 : hour % 12}:${minute.toString().padStart(2, '0')} ${Math.floor(hour / 12) === 0 ? 'AM' : 'PM'}`;

function transformCourseGQL(data: CourseAAPIResponse) {
  const instructorHistoryLookup: ProfessorLookup = {};
  const prerequisiteListLookup: CourseLookup = {};
  const prerequisiteForLookup: CourseLookup = {};
  // maps professor's ucinetid to professor basic details
  data.instructors.forEach((professor) => {
    if (professor) {
      instructorHistoryLookup[professor.ucinetid] = professor;
    }
  });
  // maps course's id to course basic details
  data.prerequisites.forEach((course) => {
    if (course) {
      prerequisiteListLookup[course.id] = course;
    }
  });
  // maps course's id to course basic details
  data.dependencies.forEach((course) => {
    if (course) {
      prerequisiteForLookup[course.id] = course;
    }
  });
  // create copy to override fields with lookups
  const course = { ...data } as unknown as CourseGQLData;
  course.instructors = instructorHistoryLookup;
  course.prerequisites = prerequisiteListLookup;
  course.dependencies = prerequisiteForLookup;

  return course;
}

/** @todo should move transformations to backend? check performance */
// transforms PPAPI gql schema to our needs
export function transformGQLData(index: SearchIndex, data: CourseAAPIResponse | ProfessorAAPIResponse) {
  if (index == 'courses') {
    return transformCourseGQL(data as CourseAAPIResponse);
  } else {
    return transformProfessorGQL(data as ProfessorAAPIResponse);
  }
}

function transformProfessorGQL(data: ProfessorAAPIResponse) {
  const courseHistoryLookup: CourseLookup = {};
  // maps course's id to course basic details
  data.courses.forEach((course) => {
    if (course) {
      courseHistoryLookup[course.id] = course;
    }
  });
  // create copy to override fields with lookups
  const professor = { ...data } as unknown as ProfessorGQLData;
  professor.courses = courseHistoryLookup;

  return professor;
}

export function useIsDesktop() {
  const isDesktop = useMediaQuery({ minWidth: 800 });
  return isDesktop;
}

export function useIsMobile() {
  const isMobile = useMediaQuery({ maxWidth: 799.9 });
  return isMobile;
}
