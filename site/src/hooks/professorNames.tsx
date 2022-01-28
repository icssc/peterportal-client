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
    shortened_name: string;
}

interface ProfessorNameLookup {
    [key: string]: ProfessorName
}

// given a course id, get the professor names listed in the instructor history
function useProfessorNames(courseID: string | undefined) {
    const query = gql`
    query GetInstructor {
        course(id: "${courseID}"){
            instructor_history{
                name
                ucinetid
                shortened_name
            }
        }
    }`;
    const { loading, error, data } = useQuery<ProfessorNameData>(query);
    if (loading || error) {
        return { loading, error, professorNameLookup: ({} as ProfessorNameLookup) };
    }
    else {
        if (!courseID) {
            return { loading, error, professorNameLookup: ({} as ProfessorNameLookup) };
        }
        let lookup: ProfessorNameLookup = {};
        data!.course.instructor_history.forEach(professor => {
            if (professor) {
                lookup[professor.ucinetid] = professor;
            }
        })
        return { loading, error, professorNameLookup: lookup };
    }
}

export { useProfessorNames }