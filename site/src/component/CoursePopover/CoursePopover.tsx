import { FC } from 'react';
import './CoursePopover.scss';
import { QuarterName } from '@peterportal/types';
import { CourseGQLData } from '../../types/types';
import { pluralize } from '../../helpers/util';
import {
  CorequisiteText,
  CourseBookmarkButton,
  CourseSynopsis,
  IncompletePrerequisiteText,
  PrerequisiteText,
  PreviousOfferingsRow,
  TermMismatchText,
} from '../CourseInfo/CourseInfo';
import LoadingSpinner from '../LoadingSpinner/LoadingSpinner';

interface CoursePopoverProps {
  course: CourseGQLData | string;
  requiredCourses?: string[];
  termMismatch?: QuarterName | 'RecentYears';
}

const CoursePopover: FC<CoursePopoverProps> = ({ course, requiredCourses, termMismatch }) => {
  if (typeof course === 'string') {
    return (
      <div className="course-popover">
        <LoadingSpinner />
      </div>
    );
  }

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
      <TermMismatchText termMismatch={termMismatch} />
    </div>
  );
};

export default CoursePopover;
