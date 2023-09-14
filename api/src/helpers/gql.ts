export function getCourseQuery(courseIDs: string[]) {
    // start of query 
    let result = 'query {';
    
    // request fields for each course
    courseIDs.forEach((courseID, i) => {
        // use number id here because cannot use special character names
        result += `
        ${'_' + i}: course(courseId: "${courseID}") {
            id
            department
            courseNumber
            school
            title
            courseLevel
            minUnits
            maxUnits
            description
            departmentName
            instructorHistory
            prerequisiteTree
            prerequisiteList
            prerequisiteText
            prerequisiteFor
            repeatability
            concurrent
            sameAs
            restriction
            overlap
            corequisites
            geList
            geText
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
            shortenedName
            ucinetid
            title
            department
            schools
            relatedDepartments
            courseHistory
        },
        `
    })
    
    // close off query
    result += '}';
    return result;
}