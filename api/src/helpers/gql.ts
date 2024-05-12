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
            instructors {
              ucinetid
              name
              shortenedName
            }
            prerequisiteTree
            prerequisites {
              id
              department
              courseNumber
              title
            }
            prerequisiteText
            dependencies {
              id
              department
              courseNumber
              title
            }
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
        `;
  });

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
            courses {
              id
              department
              courseNumber
              title
            }
        },
        `;
  });

  // close off query
  result += '}';
  return result;
}
