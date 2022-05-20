export function getCourseQuery(courseIDs: string[]) {
    // start of query 
    let result = 'query {';
    
    // request fields for each course
    courseIDs.forEach((courseID, i) => {
        // use number id here because cannot use special character names
        result += `
        ${'_' + i}: course(id: "${courseID}"){
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
        },
        `
    })
    
    // close off query
    result += '}';
    return result;
}

export function getProfessorQuery(ucinetids: string[]) {
    // start of query 
    let result = 'query {';
    
    // request fields for each course
    ucinetids.forEach((ucinetid, i) => {
        // use number id here because cannot use special character names
        result += `
        ${'_' + i}: instructor(ucinetid: "${ucinetid}"){
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
        },
        `
    })
    
    // close off query
    result += '}';
    return result;
}