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
