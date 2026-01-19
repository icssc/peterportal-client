import './ProfessorResult.scss';
import React, { FC } from 'react';
import { ProfessorGQLData } from '../../../types/types';
import { useAppDispatch } from '../../../store/hooks';
import { addPreview, clearPreviews } from '../../../store/slices/previewSlice.ts';
import { addDelimiter } from '../../../helpers/util';
import { CoursePreviewWithTerms } from '@peterportal/types';

interface RecentlyTaughtListProps {
  courses: CoursePreviewWithTerms[];
}

const RecentlyTaughtList: FC<RecentlyTaughtListProps> = ({ courses }) => {
  const dispatch = useAppDispatch();

  return (
    <>
      {addDelimiter(
        courses.slice(0, 10).map((c) => (
          <a
            key={c.id}
            href={`/course/${c.id}`}
            className="course-link"
            onClick={(e) => {
              e.preventDefault();
              dispatch(addPreview({ type: 'course', id: c.id }));
            }}
          >
            {c.department} {c.courseNumber}
          </a>
        )),
        ', ',
      )}
      {courses.length > 10 && ` + ${courses.length - 10} more...`}
    </>
  );
};

const ProfessorResult: FC<{ data: ProfessorGQLData }> = ({ data: professor }) => {
  const dispatch = useAppDispatch();

  const courses = Object.values(professor.courses);
  const hasCourses = courses.length > 0;

  const handleLinkClick = (event: React.MouseEvent) => {
    event.preventDefault();
    dispatch(clearPreviews());
    dispatch(addPreview({ type: 'professor', id: professor.ucinetid }));
  };

  return (
    <div className="professor-result">
      <a href={`/professor/${professor.ucinetid}`} className="professor-link" onClick={handleLinkClick}>
        {professor.name}
      </a>
      <p className="professor-synopsis">
        {professor.title && <span className="professor-title">{professor.title}</span>}

        {professor.title && professor.department && ' • '}

        {professor.department && <span className="professor-department">{professor.department}</span>}
      </p>
      <p className="recent-courses">
        <b>Recently Taught:</b>{' '}
        {hasCourses ? <RecentlyTaughtList courses={courses} /> : <span className="no-courses">No recent courses</span>}
      </p>
    </div>
  );
};

export default ProfessorResult;
