import { FC } from 'react';
import './CourseInfo.scss';

import CourseQuarterIndicator from '../QuarterTooltip/CourseQuarterIndicator';

import { CourseGQLData } from '../../types/types';
import { setShowAddCourse } from '../../store/slices/roadmapSlice';
import { useAppDispatch } from '../../store/hooks';
import { useSavedCourses } from '../../hooks/savedCourses';
import { useClearedCourses } from '../../hooks/planner';
import { useIsMobile, getUnitText, pluralize } from '../../helpers/util';
import { getMissingPrerequisites } from '../../helpers/planner';

import IconButton from '@mui/material/IconButton';
import BookmarkBorderIcon from '@mui/icons-material/BookmarkBorder';
import BookmarkIcon from '@mui/icons-material/Bookmark';
import WarningAmberIcon from '@mui/icons-material/WarningAmber';
import CloseIcon from '@mui/icons-material/Close';

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
  if (!course.prerequisiteText) return null;
  return (
    <p>
      <b>Prerequisites:</b> {course.prerequisiteText}
    </p>
  );
};

export const CorequisiteText: FC<CourseProp> = ({ course }) => {
  if (!course.corequisites) return null;
  return (
    <p>
      <b>Corequisites:</b> {course.corequisites}
    </p>
  );
};

interface IncompletePrerequisiteTextProps {
  missingPrerequisites?: string[];
}

export const IncompletePrerequisiteText: FC<IncompletePrerequisiteTextProps> = ({ missingPrerequisites }) => {
  if (!missingPrerequisites?.length) return null;

  return (
    <div className="course-info-warning">
      <div className="warning-primary">
        <WarningAmberIcon className="warning-primary-icon" />
        Prerequisite{pluralize(missingPrerequisites.length)} Not Met: {missingPrerequisites.join(', ')}
      </div>
      <div className="warning-hint-italics">
        Already completed? Click "Transfer Credits" at the top of the roadmap viewer to add{' '}
        {pluralize(missingPrerequisites.length, 'these prerequisites', 'this prerequisite')}.
      </div>
    </div>
  );
};

export const PreviousOfferingsRow: FC<CourseProp> = ({ course }) => {
  if (!course.terms || course.terms.length === 0) return null;
  return (
    <p className="quarter-offerings-section">
      <b>Previous Offerings:</b>
      <CourseQuarterIndicator terms={course.terms} size="sm" />
    </p>
  );
};

export const CourseHeader: FC<CourseProp> = ({ course }) => {
  const isMobile = useIsMobile();

  const dispatch = useAppDispatch();
  const closePopup = () => dispatch(setShowAddCourse(false));

  const courseName = `${course.department} ${course.courseNumber}`;
  const unitText = `(${getUnitText(course)})`;

  if (isMobile) {
    return (
      <>
        <b>{courseName}</b>
        <p>{unitText}</p>
        <CourseBookmarkButton course={course} />
        <span className="spacer" />
        <button onClick={closePopup} className="close-button">
          <CloseIcon />
        </button>
      </>
    );
  }

  return (
    <>
      <b>{courseName}</b>
      <p>{unitText}</p>
      <span className="spacer" />
      <CourseBookmarkButton course={course} />
    </>
  );
};

export const AllCourseInfo: FC<CourseProp & IncompletePrerequisiteTextProps> = ({ course, missingPrerequisites }) => {
  const isMobile = useIsMobile();
  const clearedCourses = useClearedCourses();
  const requiredCourses = getMissingPrerequisites(clearedCourses, course);

  if (isMobile) {
    return (
      <>
        <CourseDescription course={course} />
        {missingPrerequisites ? (
          <IncompletePrerequisiteText missingPrerequisites={missingPrerequisites} />
        ) : (
          <PrerequisiteText course={course} />
        )}
        <PreviousOfferingsRow course={course} />
      </>
    );
  }

  return (
    <>
      <CourseDescription course={course} />
      <PrerequisiteText course={course} />
      <CorequisiteText course={course} />
      <IncompletePrerequisiteText missingPrerequisites={requiredCourses} />
      <PreviousOfferingsRow course={course} />
    </>
  );
};
