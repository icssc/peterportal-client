import React, { FC } from "react";
import "./Course.scss";
import { Button } from "react-bootstrap";
import { InfoCircle, ExclamationTriangle, Trash } from "react-bootstrap-icons";
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import Popover from 'react-bootstrap/Popover';

import { CourseGQLData } from '../../types/types';

interface CourseProps extends CourseGQLData {
  requiredCourses?: string[];
  onDelete?: () => void;
}

const Course: FC<CourseProps> = (props) => {
  let { id, department, courseNumber, title, minUnits, maxUnits, description, prerequisiteText, corequisite, requiredCourses, onDelete } = props;

  const CoursePopover = <Popover id={'course-popover-' + id}>
    <Popover.Content>
      <div className="course-popover">
        <div className="popover-name">{department + ' ' + courseNumber} {title}</div>
        <div className="popover-units">
          <span className="popover-units-value">{minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`}</span> units
        </div>
        <div className="popover-description">{description}</div>
        {prerequisiteText && <div className="popover-detail">
          <span className="popover-detail-prefix">Prerequisite:</span> {prerequisiteText}
        </div>}
        {corequisite && <div className="popover-detail">
          <span className="popover-detail-prefix">Corequisite:</span> {corequisite}
        </div>}
      </div>
    </Popover.Content>
  </Popover>

  const WarningPopover = <Popover id={'warning-popover-' + id}>
    <Popover.Content>
      Prerequisite not met! Missing: {requiredCourses?.join(', ')}
    </Popover.Content>
  </Popover>

  const courseRoute = () => {
    return '/course/' + props.department.replace(/\s+/g, '') + props.courseNumber.replace(/\s+/g, '')
  }

  return (
    <div className={`course ${requiredCourses ? 'invalid' : ''}`}>
      <div className="course-card-top">
        <div className="course-and-info">
          <a className="name" href={courseRoute()} target="_blank" rel="noopener noreferrer">{department + ' ' + courseNumber}</a>
          <OverlayTrigger
            trigger={['hover', 'focus']}
            placement="auto"
            overlay={CoursePopover}
            delay={100}>
            <InfoCircle className="info-circle" />
          </OverlayTrigger>
        </div>
        {onDelete && (
          <Button variant="light" className="course-delete-btn" onClick={onDelete}>
            <Trash className="course-delete-icon" />
          </Button>
        )}
      </div>
      <div className="title">{title}</div>
      <div className="footer">
        {requiredCourses && <OverlayTrigger
          trigger={['hover', 'focus']}
          placement="right"
          overlay={WarningPopover}
          delay={100}>
          <ExclamationTriangle />
        </OverlayTrigger>}
        <div className="units">{minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`} units</div>
      </div>
    </div>
  );
};

export default Course;
