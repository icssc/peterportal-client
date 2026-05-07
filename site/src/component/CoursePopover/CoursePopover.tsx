import { FC } from 'react';
import './CoursePopover.scss';
import { CourseGQLData } from '../../types/types';
import { pluralize } from '../../helpers/util';
import {
  CorequisiteText,
  CourseBookmarkButton,
  CourseSynopsis,
  IncompletePrerequisiteText,
  PrerequisiteText,
  PreviousOfferingsRow,
  QuarterMismatchText,
} from '../CourseInfo/CourseInfo';
import LoadingSpinner from '../LoadingSpinner/LoadingSpinner';
import { useClearedCoursesUntil } from '../../hooks/planner';
import { getMissingPrerequisites } from '../../helpers/planner';

interface CoursePopoverProps {
  course: CourseGQLData | string;
  requiredCourses?: string[];
  quarterMismatch?: string;
}

const CoursePopover: FC<CoursePopoverProps> = ({ course, requiredCourses, quarterMismatch }) => {
  const courseId = typeof course === 'string' ? '' : `${course.department} ${course.courseNumber}`;
  const clearedCourses = useClearedCoursesUntil(courseId);

  if (typeof course === 'string') {
    return (
      <div className="course-popover">
        <LoadingSpinner />
      </div>
    );
  }

  requiredCourses = getMissingPrerequisites(clearedCourses, course.prerequisiteTree);
  const { department, courseNumber, minUnits, maxUnits } = course;

  return (
    <div className="course-popover">
      <div className="popover-name">
        {department + ' ' + courseNumber + ' '}
        <span className="popover-units">
          ({minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`} {pluralize(maxUnits, 'units', 'unit')})
        </span>
        <div className="spacer" />
        <CourseBookmarkButton course={course} />
      </div>
      <br />
      <CourseSynopsis course={course} />
      <PrerequisiteText course={course} />
      <CorequisiteText course={course} />
      <IncompletePrerequisiteText requiredCourses={requiredCourses} />
      <PreviousOfferingsRow course={course} />
      <QuarterMismatchText quarterMismatch={quarterMismatch} />
    </div>
  );
};

export default CoursePopover;
