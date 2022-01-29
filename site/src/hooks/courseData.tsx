import { gql, useQuery } from '@apollo/client';
import { CourseGQLData, SubProfessor, SubCourse, ProfessorLookup, CourseLookup } from '../types/types';

interface CourseGQLResponse {
    course: Omit<CourseGQLData, 'instructor_history' | 'prerequisite_list' | 'prerequisite_for'> & {
        instructor_history: SubProfessor[];
        prerequisite_list: SubCourse[];
        prerequisite_for: SubCourse[];
    }
}

const placeholder: CourseGQLData = {
    id: '',
    department: '',
    number: '',
    school: '',
    title: '',
    course_level: '',
    department_alias: [],
    units: [0, 0],
    description: '',
    department_name: '',
    instructor_history: {},
    prerequisite_tree: '',
    prerequisite_list: {},
    prerequisite_text: '',
    prerequisite_for: {},
    repeatability: '',
    concurrent: '',
    same_as: '',
    restriction: '',
    overlap: '',
    corequisite: '',
    ge_list: [],
    ge_text: '',
    terms: []
}

// given a course id, get the gql equivalent
function useCourseGQL(courseID: string | undefined) {
    const query = gql`
    query {
        course(id: "${courseID}"){
    		id
    		department
            number
            school
            title
            course_level
            department_alias
            units
            description
            department_name
            instructor_history{
                name
                ucinetid
                shortened_name
            }
            prerequisite_tree
            prerequisite_list {
                id
                department
                number
                title
            }
            prerequisite_text
            prerequisite_for {
                id
                department
                number
                title        
            }
            repeatability
            concurrent
            same_as
            restriction
            overlap
            corequisite
            ge_list
            ge_text
            terms
        }
    }`;
    const { loading, error, data } = useQuery<CourseGQLResponse>(query);
    if (loading || error) {
        return { loading, error, course: placeholder };
    }
    else {
        if (!courseID || !data?.course) {
            return { loading, error, course: placeholder };
        }
        let instructorHistoryLookup: ProfessorLookup = {};
        let prerequisiteListLookup: CourseLookup = {};
        let prerequisiteForLookup: CourseLookup = {};
        // maps professor's ucinetid to professor basic details
        data!.course.instructor_history.forEach(professor => {
            if (professor) {
                instructorHistoryLookup[professor.ucinetid] = professor;
            }
        })
        // maps course's id to course basic details
        data!.course.prerequisite_list.forEach(course => {
            if (course) {
                prerequisiteListLookup[course.id] = course;
            }
        })
        // maps course's id to course basic details
        data!.course.prerequisite_for.forEach(course => {
            if (course) {
                prerequisiteForLookup[course.id] = course;
            }
        })
        // create copy to override fields with lookups
        let course = { ...data!.course } as unknown as CourseGQLData;
        course.instructor_history = instructorHistoryLookup;
        course.prerequisite_list = prerequisiteListLookup;
        course.prerequisite_for = prerequisiteForLookup;
        return { loading, error, course: course };
    }
}

export { useCourseGQL }