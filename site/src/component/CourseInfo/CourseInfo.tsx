import { FC } from 'react';
import { CourseGQLData } from '../../types/types';
import { useCoursebag } from '../../hooks/coursebag';
import { Bookmark, BookmarkFill, ExclamationTriangle } from 'react-bootstrap-icons';
import { pluralize } from '../../helpers/util';

interface CourseProp {
  course: CourseGQLData;
}

export const CourseBookmarkButton: FC<CourseProp> = ({ course }) => {
  const { coursebag: bookmarks, toggleBookmark } = useCoursebag();
  const isBookmarked = bookmarks.some((c) => c.id === course.id);

  return (
    <button className="unstyled" onClick={() => toggleBookmark(course)}>
      {isBookmarked ? <BookmarkFill /> : <Bookmark />}
    </button>
  );
};

export const CourseDescription: FC<CourseProp> = ({ course }) => {
  return (
    <p>
      <b>{course.title}:</b> {course.description}
    </p>
  );
};

export const PrerequisiteText: FC<CourseProp> = ({ course }) => {
  if (!course.prerequisiteText) return <></>;

  return (
    <p>
      <b>Prerequisites:</b> {course.prerequisiteText}
    </p>
  );
};

export const CorequisiteText: FC<CourseProp> = ({ course }) => {
  if (!course.corequisites) return <></>;

  return (
    <p>
      <b>Corequisites:</b> {course.corequisites}
    </p>
  );
};

export const IncompletePrerequisiteText: FC<{ requiredCourses?: string[] }> = ({ requiredCourses }) => {
  if (!requiredCourses?.length) return;

  return (
    <div>
      <div className="popover-detail-warning">
        <ExclamationTriangle className="popover-detail-warning-icon" />
        Prerequisite{pluralize(requiredCourses.length)} Not Met: {requiredCourses.join(', ')}
      </div>
      <div className="popover-detail-italics">
        Already completed? Click "Transfer Credits" at the top of the roadmap viewer to add{' '}
        {pluralize(requiredCourses.length, 'these prerequisites', 'this prerequisite')}.
      </div>
    </div>
  );
};
