import './ProfessorResult.scss';
import React, { FC } from 'react';
import { ProfessorGQLData } from '../../../types/types';
import { useAppDispatch } from '../../../store/hooks';
import { addPreview } from '../../../store/slices/previewSlice';
import { addDelimiter } from '../../../helpers/util';
import Link from 'next/link';
import { CoursePreviewWithTerms } from '@peterportal/types';
import { useRouter } from 'next/navigation';

interface RecentlyTaughtListProps {
  courses: CoursePreviewWithTerms[];
}

const RecentlyTaughtList: FC<RecentlyTaughtListProps> = ({ courses }) => {
  const dispatch = useAppDispatch();
  const router = useRouter();

  return (
    <>
      {addDelimiter(
        courses.slice(0, 10).map((c) => (
          <Link
            key={c.id}
            href={`/course/${c.id}`}
            className="course-link"
            onClick={(e) => {
              e.preventDefault();
              router.push(`?course=${encodeURIComponent(c.id)}`);
              dispatch(addPreview({ type: 'course', id: c.id }));
            }}
          >
            {c.department} {c.courseNumber}
          </Link>
        )),
        ', ',
      )}
      {courses.length > 10 && ` + ${courses.length - 10} more...`}
    </>
  );
};

const ProfessorResult: FC<{ data: ProfessorGQLData }> = ({ data: professor }) => {
  const dispatch = useAppDispatch();
  const router = useRouter();

  const courses = Object.values(professor.courses);
  const hasCourses = courses.length > 0;

  const handleLinkClick = (event: React.MouseEvent) => {
    event.preventDefault();
<<<<<<< preview-navigation
    router.push(`?instructor=${encodeURIComponent(professor.ucinetid)}`);
    dispatch(addPreview({ type: 'professor', id: professor.ucinetid }));
=======
    dispatch(clearPreviews());
    dispatch(addPreview({ type: 'instructor', id: professor.ucinetid }));
>>>>>>> main
  };

  return (
    <div className="professor-result">
      <Link href={`/instructor/${professor.ucinetid}`} className="professor-link" onClick={handleLinkClick}>
        {professor.name}
      </Link>
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
