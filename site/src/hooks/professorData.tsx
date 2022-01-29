import { gql, useQuery } from '@apollo/client';
import { ProfessorGQLData, SubCourse, CourseLookup } from '../types/types';

interface ProfessorGQLResponse {
    instructor: Omit<ProfessorGQLData, 'course_history'> & {
        course_history: SubCourse[];
    }
}

const placeholder: ProfessorGQLData = {
    name: '',
    shortened_name: '',
    ucinetid: '',
    title: '',
    department: '',
    schools: [],
    related_departments: [],
    course_history: {}
}

// given a course id, get the gql equivalent
function useProfessorGQL(professorID: string | undefined) {
    const query = gql`
    query {
        instructor(ucinetid:"${professorID}"){
            name
            shortened_name
            ucinetid
            title
            department
            schools
            related_departments
            course_history {
                id
                department
                number
                title
            }
        }
    }`;
    const { loading, error, data } = useQuery<ProfessorGQLResponse>(query);
    if (loading || error) {
        return { loading, error, professor: placeholder };
    }
    else {
        if (!professorID || !data?.instructor) {
            return { loading, error, professor: placeholder };
        }
        let courseHistoryLookup: CourseLookup = {};
        // maps course's id to course basic details
        data!.instructor.course_history.forEach(course => {
            if (course) {
                courseHistoryLookup[course.id] = course;
            }
        })
        // create copy to override fields with lookups
        let professor = { ...data!.instructor } as unknown as ProfessorGQLData;
        professor.course_history = courseHistoryLookup;
        return { loading, error, professor: professor };
    }
}

export { useProfessorGQL }