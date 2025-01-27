import { FC } from 'react';
import { CourseGQLData } from '../../types/types';
import { useCoursebag } from '../../hooks/coursebag';
import { Bookmark, BookmarkFill } from 'react-bootstrap-icons';

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
