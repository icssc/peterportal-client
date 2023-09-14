import axios from 'axios';
import { SearchIndex, CourseGQLData, CourseGQLResponse, ProfessorGQLData, ProfessorGQLResponse, ProfessorLookup, CourseLookup, BatchCourseData, BatchProfessorData, SearchType } from '../types/types';

export function getCourseTags(course: CourseGQLData) {
  // data to be displayed in pills
  let tags: string[] = [];
  // course level
  let courseLevel = course.courseLevel;
  if (courseLevel) {
    tags.push(`${courseLevel.substring(0, courseLevel.indexOf('('))}`);
  }
  // ge
  course.geList.forEach(ge => {
    tags.push(`${ge.substring(0, ge.indexOf(':'))}`);
  })
  // units
  const { minUnits, maxUnits } = course;
  tags.push(`${minUnits === maxUnits ? maxUnits : `${minUnits}-${maxUnits}`} unit${(minUnits === maxUnits ? (maxUnits !== 1 ? 's' : '') : 's')}`);
  return tags;
}

// helper function to search 1 result from course/professor page
export function searchAPIResult(type: SearchType, name: string) {
  return new Promise<CourseGQLData | ProfessorGQLData | undefined>(res => {
    let index: SearchIndex;
    if (type === 'course') {
      index = 'courses';
    }
    else {
      index = 'professors';
    }
    searchAPIResults(index, [name])
      .then(results => {
        if (Object.keys(results).length > 0) {
          let key = Object.keys(results)[0];
          res(results[key]);
        }
        else {
          res(undefined)
        }
      })
  })
}

// helper function to query from API and transform to data used in redux
export function searchAPIResults(index: SearchIndex, names: string[]) {
  return new Promise<BatchCourseData | BatchProfessorData>(res => {
    // Get results from backend search
    axios.post<{ [key: string]: CourseGQLResponse | ProfessorGQLResponse }>(`/api/${index}/api/batch`, { [index]: names })
      .then(searchResponse => {
        let data = searchResponse.data;
        let transformed: BatchCourseData | BatchProfessorData = {};
        Object.keys(data).forEach(id => {
          // filter out null reponses
          if (data[id]) {
            // use specific key based on index
            let key = ''
            if (index == 'courses') {
              key = (data[id] as CourseGQLResponse).id;
            }
            else {
              key = (data[id] as ProfessorGQLResponse).ucinetid;
            }
            // perform transformation
            transformed[key] = transformGQLData(index, data[id])
          }
        })
        console.log('From backend search', transformed);
        res(transformed);
      })
  })
}

// transforms PPAPI gql schema to our needs
export function transformGQLData(index: SearchIndex, data: CourseGQLResponse | ProfessorGQLResponse) {
  if (index == 'courses') {
    return transformCourseGQL(data as CourseGQLResponse);
  }
  else {
    return transformProfessorGQL(data as ProfessorGQLResponse);
  }
}

function transformCourseGQL(data: CourseGQLResponse) {
  let instructorHistoryLookup: ProfessorLookup = {};
  let prerequisiteListLookup: CourseLookup = {};
  let prerequisiteForLookup: CourseLookup = {};
  axios.post<{ [key: string]: CourseGQLResponse }>
  (`/api/courses/api/batch`, {"courses": data.prerequisiteList.map((x) => x.replace(/ /g, ""))})
    .then(r => prerequisiteListLookup = r.data);
  axios.post<{ [key: string]: CourseGQLResponse }>
  (`/api/courses/api/batch`, {"courses": data.prerequisiteFor.map((x) => x.replace(/ /g, ""))})
    .then(r => prerequisiteForLookup = r.data);
  axios.post<{ [key: string]: ProfessorGQLResponse }>
  (`/api/professors/api/batch`, {"professors": data.instructorHistory})
    .then(r => instructorHistoryLookup = r.data);
   // create copy to override fields with lookups
  let course = { ...data } as unknown as CourseGQLData;
  course.instructorHistory = instructorHistoryLookup;
  course.prerequisiteList = prerequisiteListLookup;
  course.prerequisiteFor = prerequisiteForLookup;
  return course;
}

function transformProfessorGQL(data: ProfessorGQLResponse) {
  let courseHistoryLookup: CourseLookup = {};
  axios.post<{ [key: string]: CourseGQLResponse }>
  (`/api/courses/api/batch`, {"courses": Object.keys(data.courseHistory).map((x) => x.replace(/ /g, ""))})
    .then(r => courseHistoryLookup = r.data);
  // create copy to override fields with lookups
  let professor = { ...data } as unknown as ProfessorGQLData;
  professor.courseHistory = courseHistoryLookup;

  return professor;
}

export const hourMinuteTo12HourString = ({ hour, minute }: { hour: number, minute: number }) =>
  `${hour % 12}:${minute.toString().padStart(2, "0")} ${Math.floor(hour / 12) === 0 ? "AM" : "PM"}`;
