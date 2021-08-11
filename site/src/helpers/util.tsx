import { gql } from '@apollo/client';

export interface GetProfessorNameData {
    course: {
        instructor_history: Array<ProfessorName>
    }
}

interface ProfessorName {
    name: string;
    ucinetid: string;
}

function getProfessorNamesFromCourse(courseID: string | undefined) {
    const query = gql`
    query GetInstructor {
        course(id: "${courseID}"){
            instructor_history{
                ucinetid
                name
            }
        }
    }`;
    return query;
}

export { getProfessorNamesFromCourse }