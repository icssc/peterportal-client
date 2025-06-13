import { FC } from 'react';
import { CourseGQLData } from '../../types/types';
import { useSavedCourses } from '../../hooks/savedCourses';
import { pluralize } from '../../helpers/util';
import './CourseInfo.scss';
import CourseQuarterIndicator from '../QuarterTooltip/CourseQuarterIndicator';

import { IconButton } from '@mui/material';
import BookmarkBorderIcon from '@mui/icons-material/BookmarkBorder';
import BookmarkIcon from '@mui/icons-material/Bookmark';
import WarningAmberIcon from '@mui/icons-material/WarningAmber';

interface CourseProp {
  course: CourseGQLData;
}

export const CourseBookmarkButton: FC<CourseProp> = ({ course }) => {
  const { isCourseSaved, toggleSavedCourse } = useSavedCourses();
  const courseIsSaved = isCourseSaved(course);
  return (
    <IconButton onClick={() => toggleSavedCourse(course)}>
      {courseIsSaved ? <BookmarkIcon /> : <BookmarkBorderIcon />}
    </IconButton>
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
    <div className="course-info-warning">
      <div className="warning-primary">
        <WarningAmberIcon className="warning-primary-icon" />
        Prerequisite{pluralize(requiredCourses.length)} Not Met: {requiredCourses.join(', ')}
      </div>
      <div className="warning-hint-italics">
        Already completed? Click "Transfer Credits" at the top of the roadmap viewer to add{' '}
        {pluralize(requiredCourses.length, 'these prerequisites', 'this prerequisite')}.
      </div>
    </div>
  );
};

export const PreviousOfferingsRow: FC<CourseProp> = ({ course }) => {
  return (
    <>
      {course.terms && course.terms.length > 0 && (
        <p className="quarter-offerings-section">
          <b>Previous Offerings:</b>
          <CourseQuarterIndicator terms={course.terms} size="sm" />
        </p>
      )}
    </>
  );
};
