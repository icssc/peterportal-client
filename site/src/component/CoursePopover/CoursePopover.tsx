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
} from '../CourseInfo/CourseInfo';
import { Spinner } from 'react-bootstrap';
import CourseQuarterIndicator from '../QuarterTooltip/CourseQuarterIndicator';

interface CoursePopoverProps {
  course: CourseGQLData | string;
  requiredCourses?: string[];
  interactive?: boolean;
}

const CoursePopover: FC<CoursePopoverProps> = ({ course, interactive = true, requiredCourses }) => {
  let content = (
    <div className="center">
      <Spinner animation="border" />
    </div>
  );

  if (typeof course !== 'string') {
    const { department, courseNumber, minUnits, maxUnits } = course;
    content = (
      <>
        <div className="popover-name">
          {department + ' ' + courseNumber + ' '}
          <span className="popover-units">
            ({minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`} {pluralize(maxUnits, 'units', 'unit')})
          </span>

          <div className="spacer"></div>
          {interactive && <CourseBookmarkButton course={course} />}
        </div>
        <br />
        <CourseDescription course={course} />
        <PrerequisiteText course={course} />
        <CorequisiteText course={course} />
        <IncompletePrerequisiteText requiredCourses={requiredCourses} />
        {course.terms && course.terms.length > 0 && (
          <div className="quarter-offerings">
            <b>Quarter Offerings:</b>
            <div className="quarter-indicator-wrapper">
              <CourseQuarterIndicator terms={course.terms} size="sm" />
            </div>
          </div>
        )}
      </>
    );
  }
  return <Popover.Content className="course-popover">{content}</Popover.Content>;
};

export default CoursePopover;
