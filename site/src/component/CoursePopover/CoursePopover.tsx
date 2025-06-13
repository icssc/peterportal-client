import { FC } from 'react';
import './CoursePopover.scss';
import Popover from 'react-bootstrap/Popover';
import { CourseGQLData } from '../../types/types';
import { getUnitText } from '../../helpers/util';
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

const CoursePopoverContent: FC<CoursePopoverProps> = ({ course, requiredCourses }) => {
  const clearedCourses = useClearedCourses();

  if (typeof course === 'string') {
    return <LoadingSpinner />;
  }

  requiredCourses = getMissingPrerequisites(clearedCourses, course);
  const unitText = getUnitText(course);

  return (
    <>
      <div className="popover-name">
        <b>{`${course.department} ${course.courseNumber} `}</b>
        <p>({unitText})</p>
        <span className="spacer" />
        <CourseBookmarkButton course={course} />
      </div>
      <CourseDescription course={course} />
      <PrerequisiteText course={course} />
      <CorequisiteText course={course} />
      <IncompletePrerequisiteText requiredCourses={requiredCourses} />
      <PreviousOfferingsRow course={course} />
    </>
  );
};

const CoursePopover: FC<CoursePopoverProps> = ({ course, requiredCourses }) => {
  return (
    <Popover.Content className="course-popover">
      <CoursePopoverContent course={course} requiredCourses={requiredCourses} />
    </Popover.Content>
  );
};

export default CoursePopover;
