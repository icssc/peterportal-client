import { FC } from 'react';
import './CoursePopover.scss';
import { ExclamationTriangle } from 'react-bootstrap-icons';
import Popover from 'react-bootstrap/Popover';
import { CourseGQLData } from '../../types/types';
import { CourseBookmarkButton, CourseDescription } from '../CourseInfo/CourseInfo';

interface CoursePopoverProps {
  course: CourseGQLData;
  requiredCourses?: string[];
  interactive?: boolean;
}

const CoursePopover: FC<CoursePopoverProps> = ({ course, interactive = true, requiredCourses }) => {
  const { department, courseNumber, minUnits, maxUnits, prerequisiteText, corequisites } = course;

  return (
    <Popover.Content className="course-popover">
      <div className="popover-name">
        {department + ' ' + courseNumber + ' '}
        <span className="popover-units">({minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`} units)</span>

        <div className="spacer"></div>
        {interactive && <CourseBookmarkButton course={course} />}
      </div>
      <br />
      <CourseDescription course={course} />
      {prerequisiteText && (
        <div className="popover-detail">
          <span className="popover-detail-prefix">Prerequisites:</span> {prerequisiteText}
        </div>
      )}
      {corequisites && (
        <div className="popover-detail">
          <span className="popover-detail-prefix">Corequisites:</span> {corequisites}
        </div>
      )}
      {requiredCourses && (
        <div className="popover-detail">
          <div className="popover-detail-warning">
            <ExclamationTriangle className="popover-detail-warning-icon" />
            Prerequisite{requiredCourses?.length > 1 ? 's' : ''} Not Met: {requiredCourses?.join(', ')}
          </div>
          <div className="popover-detail-italics">
            Already completed? Click "Transfer Credits" at the top of the roadmap viewer to add{' '}
            {requiredCourses?.length > 1 ? 'these prerequisites' : 'this prerequisite'}.
          </div>
        </div>
      )}
    </Popover.Content>
  );
};

export default CoursePopover;
