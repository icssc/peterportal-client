import './ProfessorResult.scss';
import React, { FC } from 'react';
import { ProfessorGQLData } from '../../../types/types';
import { useAppDispatch } from '../../../store/hooks';
import { setPreviewedCourse, setPreviewedProfessor } from '../../../store/slices/coursePreviewSlice';
import { addDelimiter } from '../../../helpers/util';
import Link from 'next/link';

const ProfessorResult: FC<{ data: ProfessorGQLData }> = ({ data: professor }) => {
  const dispatch = useAppDispatch();

  /** @todo make recent courses only the ones taught within the last 5 years */
  const courses = Object.values(professor.courses);

  const handleLinkClick = (event: React.MouseEvent) => {
    event.preventDefault();
    dispatch(setPreviewedProfessor(professor.ucinetid));
  };

  return (
    <div className="professor-result">
      <Link href={`/professor/${professor.ucinetid}`} className="professor-link" onClick={handleLinkClick}>
        {professor.name}
      </Link>
      <p className="professor-synopsis">
        <span className="professor-title">{professor.title}</span>
        {' • '}
        <span className="professor-department">{professor.department}</span>
      </p>
      <p className="recent-courses">
        <b>Recently Taught:</b>{' '}
        {addDelimiter(
          courses.slice(0, 10).map((c) => {
            const handleLinkClick = (event: React.MouseEvent) => {
              event.preventDefault();
              dispatch(setPreviewedCourse(c.id));
            };

            return (
              <a key={c.id} href={`/course/${c.id}`} className="course-link" onClick={handleLinkClick}>
                {c.department} {c.courseNumber}
              </a>
            );
          }),
          ', ',
        )}{' '}
        {courses.length > 10 && ` + ${courses.length - 10} more...`}
      </p>
    </div>
  );
};

export default ProfessorResult;
