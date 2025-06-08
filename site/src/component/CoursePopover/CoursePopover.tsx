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
  interactive?: boolean;
}

const CoursePopoverContent: FC<CoursePopoverProps> = ({ course, requiredCourses, interactive }) => {
  const clearedCourses = useClearedCourses();

  if (typeof course === 'string') {
    return <LoadingSpinner />;
  }

  requiredCourses = getMissingPrerequisites(clearedCourses, course);
  const unitText = getUnitText(course);

  return (
    <>
      <div className="popover-name">
        {`${course.department} ${course.courseNumber} `}
        <span className="popover-units">({unitText})</span>
        <span className="spacer" />
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
      <CoursePopoverContent course={course} requiredCourses={requiredCourses} interactive={interactive} />
    </Popover.Content>
  );
};

export default CoursePopover;
