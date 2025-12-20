import './ProfessorResult.scss';
import React, { FC } from 'react';
import { ProfessorGQLData } from '../../../types/types';
import { useAppDispatch } from '../../../store/hooks';
import { setPreviewedCourse, setPreviewedProfessor } from '../../../store/slices/coursePreviewSlice';
import { addDelimiter } from '../../../helpers/util';

const ProfessorResult: FC<{ data: ProfessorGQLData }> = ({ data: professor }) => {
  const dispatch = useAppDispatch();

  /** @todo make recent courses only the ones taught within the last 5 years */
  const courses = Object.values(professor.courses);
  const hasCourses = courses.length > 0;

  const handleLinkClick = (event: React.MouseEvent) => {
    event.preventDefault();
    dispatch(setPreviewedProfessor(professor.ucinetid));
  };

  return (
    <div className="professor-result">
      <a href={`/professor/${professor.ucinetid}`} className="professor-link" onClick={handleLinkClick}>
        {professor.name}
      </a>
      <p className="professor-synopsis">
        <span className="professor-title">{professor.title}</span>
        {' • '}
        <span className="professor-department">{professor.department}</span>
      </p>
      <p className="recent-courses">
        <b>Recently Taught:</b>{' '}
        {hasCourses ? (
          <>
            {addDelimiter(
              courses.slice(0, 10).map((c) => (
                <a
                  key={c.id}
                  href={`/course/${c.id}`}
                  className="course-link"
                  onClick={(e) => {
                    e.preventDefault();
                    dispatch(setPreviewedCourse(c.id));
                  }}
                >
                  {c.department} {c.courseNumber}
                </a>
              )),
              ', ',
            )}
            {courses.length > 10 && ` + ${courses.length - 10} more...`}
          </>
        ) : (
          <span className="no-courses">No recent courses</span>
        )}
      </p>
    </div>
  );
};

export default ProfessorResult;
