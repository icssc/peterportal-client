import { FC } from 'react';
import Popover from 'react-bootstrap/Popover';
import './CoursePopover.scss';

import {
  CorequisiteText,
  CourseBookmarkButton,
  CourseDescription,
  IncompletePrerequisiteText,
  PrerequisiteText,
  PreviousOfferingsRow,
} from '../CourseInfo/CourseInfo';
import LoadingSpinner from '../LoadingSpinner/LoadingSpinner';

import { CourseGQLData } from '../../types/types';
import { useClearedCourses } from '../../hooks/planner';
import { getMissingPrerequisites } from '../../helpers/planner';
import { pluralize } from '../../helpers/util';

interface CoursePopoverProps {
  course: CourseGQLData | string;
  requiredCourses?: string[];
  interactive?: boolean;
}

const CoursePopoverContent: FC<CoursePopoverProps> = ({ course, requiredCourses, interactive }) => {
  const clearedCourses = useClearedCourses();

  if (typeof course === 'string') {
    return <LoadingSpinner />;
  }

  const { department, courseNumber, minUnits, maxUnits } = course;
  requiredCourses = getMissingPrerequisites(clearedCourses, course);
  const units = minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`;
  const unitsText = `${units} ${pluralize(maxUnits, 'units', 'unit')}`;

  return (
    <>
      <div className="popover-name">
        {`${department} ${courseNumber} `}
        <span className="popover-units">({unitsText})</span>
        <div className="spacer"></div>
        {interactive && <CourseBookmarkButton course={course} />}
      </div>
      <br />
      <CourseDescription course={course} />
      <PrerequisiteText course={course} />
      <CorequisiteText course={course} />
      <IncompletePrerequisiteText requiredCourses={requiredCourses} />
      <PreviousOfferingsRow course={course} />
    </>
  );
};

const CoursePopover: FC<CoursePopoverProps> = ({ course, requiredCourses, interactive = true }) => {
  return (
    <Popover.Content className="course-popover">
      <CoursePopoverContent course={course} interactive={interactive} requiredCourses={requiredCourses} />
    </Popover.Content>
  );
};

export default CoursePopover;
