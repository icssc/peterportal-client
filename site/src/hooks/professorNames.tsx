import { useState, useEffect } from 'react';
import { gql, useQuery } from '@apollo/client';

interface ProfessorNameData {
    course: {
        instructor_history: ProfessorName[]
    }
}

interface ProfessorName {
    name: string;
    ucinetid: string;
}

function useProfessorNames(courseID: string | undefined) {
    const query = gql`
    query GetInstructor {
        course(id: "${courseID}"){
            instructor_history{
                ucinetid
                name
            }
        }
    }`;
    const { loading, error, data } = useQuery<ProfessorNameData>(query);
    if (loading || error) {
        return { loading, error, professorNames: ([] as ProfessorName[]) };
    }
    else {
        return { loading, error, professorNames: data!.course.instructor_history.filter(x => x) };
    }
}

export { useProfessorNames }