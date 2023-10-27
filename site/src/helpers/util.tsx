import axios from 'axios';
import { SearchIndex, CourseGQLData, CourseGQLResponse, ProfessorGQLData, ProfessorGQLResponse, ProfessorLookup, CourseLookup, BatchCourseData, BatchProfessorData, SearchType } from '../types/types';

export function getCourseTags(course: CourseGQLData) {
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

// helper function to search 1 result from course/professor page
export function searchAPIResult(type: SearchType, name: string) {
  return new Promise<CourseGQLData | ProfessorGQLData | undefined>(res => {
    let index: SearchIndex;
    if (type == 'course') {
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
  // maps professor's ucinetid to professor basic details
  data.instructor_history.forEach(professor => {
    if (professor) {
      instructorHistoryLookup[professor.ucinetid] = professor;
    }
  })
  // maps course's id to course basic details
  data.prerequisite_list.forEach(course => {
    if (course) {
      prerequisiteListLookup[course.id] = course;
    }
  })
  // maps course's id to course basic details
  data.prerequisite_for.forEach(course => {
    if (course) {
      prerequisiteForLookup[course.id] = course;
    }
  })
  // create copy to override fields with lookups
  let course = { ...data } as unknown as CourseGQLData;
  course.instructor_history = instructorHistoryLookup;
  course.prerequisite_list = prerequisiteListLookup;
  course.prerequisite_for = prerequisiteForLookup;

  return course;
}

function transformProfessorGQL(data: ProfessorGQLResponse) {
  let courseHistoryLookup: CourseLookup = {};
  // maps course's id to course basic details
  data.course_history.forEach(course => {
    if (course) {
      courseHistoryLookup[course.id] = course;
    }
  })
  // create copy to override fields with lookups
  let professor = { ...data } as unknown as ProfessorGQLData;
  professor.course_history = courseHistoryLookup;

  return professor;
}

// timestamp helper
export const getTimestamp = () => {
  return new Date().getTime();
}