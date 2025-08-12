import { FC } from 'react';
import './CoursePopover.scss';
import Popover from 'react-bootstrap/Popover';
import { CourseGQLData } from '../../types/types';
import { pluralize } from '../../helpers/util';
import {
  CorequisiteText,
  CourseBookmarkButton,
  CourseDescription,
  IncompletePrerequisiteText,
  PrerequisiteText,
  PreviousOfferingsRow,
} from '../CourseInfo/CourseInfo';
import LoadingSpinner from '../LoadingSpinner/LoadingSpinner';
import { useClearedCourses } from '../../hooks/planner';
import { getMissingPrerequisites } from '../../helpers/planner';

interface CoursePopoverProps {
  course: CourseGQLData | string;
  requiredCourses?: string[];
}

const CoursePopover: FC<CoursePopoverProps> = ({ course, requiredCourses }) => {
  const clearedCourses = useClearedCourses();

  if (typeof course === 'string') {
    return (
      <Popover.Content className="course-popover">
        <LoadingSpinner />
      </Popover.Content>
    );
  }

  requiredCourses = getMissingPrerequisites(clearedCourses, course);
  const { department, courseNumber, minUnits, maxUnits } = course;

  return (
    <Popover.Content className="course-popover">
      <div className="popover-name">
        {department + ' ' + courseNumber + ' '}
        <span className="popover-units">
          ({minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`} {pluralize(maxUnits, 'units', 'unit')})
        </span>
        <div className="spacer" />
        <CourseBookmarkButton course={course} />
      </div>
      <br />
      <CourseDescription course={course} />
      <PrerequisiteText course={course} />
      <CorequisiteText course={course} />
      <IncompletePrerequisiteText requiredCourses={requiredCourses} />
      <PreviousOfferingsRow course={course} />
    </Popover.Content>
  );
};

export default CoursePopover;
