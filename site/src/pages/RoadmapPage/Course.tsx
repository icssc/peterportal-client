import React, { FC } from "react";
import "./Course.scss";
import { Button } from "react-bootstrap";
import { InfoCircle, ExclamationTriangle, Trash } from "react-bootstrap-icons";
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import Popover from 'react-bootstrap/Popover';

import { CourseData } from '../../types/types';

interface CourseProps extends CourseData {
  requiredCourses?: string[];
  onDelete?: () => void;
}

const Course: FC<CourseProps> = (props) => {
  let { id, department, number, title, units, description, prerequisite_text, corequisite, requiredCourses, onDelete } = props;

  const CoursePopover = <Popover id={'course-popover-' + id}>
    <Popover.Content>
      <div className="course-popover">
        <div className="popover-name">{department + ' ' + number} {title}</div>
        <div className="popover-units">
          <span className="popover-units-value">{units[0]}</span> units
        </div>
        <div className="popover-description">{description}</div>
        {prerequisite_text && <div className="popover-detail">
          <span className="popover-detail-prefix">Prequisite:</span> {prerequisite_text}
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

  return (
    <div className={`course ${requiredCourses ? 'invalid' : ''}`}>
      <div className="course-card-top">
        <div className="course-and-info">
          <div className="name">{department + ' ' + number}</div>
          <OverlayTrigger
            trigger={['hover', 'focus']}
            placement="left"
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
        <div className="units">{units[0]} units</div>
      </div>
    </div>
  );
};

export default Course;
