import React, { FC } from "react";
import "./Course.scss";
import { InfoCircle, ExclamationTriangle } from "react-bootstrap-icons";
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import Popover from 'react-bootstrap/Popover';

import { CourseData } from '../../types/types';

interface CourseProps extends CourseData {
  invalid?: boolean;
}

const Course: FC<CourseProps> = (props) => {
  let { id, department, number, title, units, description, prerequisite_text, corequisite, invalid } = props;

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
      You do not meet the prerequisite to take this class.
    </Popover.Content>
  </Popover>

  return (
    <div className={`course ${invalid ? 'invalid' : ''}`}>
      <div className="course-card-top">
        <div className="name">{department + ' ' + number}</div>
        <div className="units">{units[0]} units</div>
      </div>

      <div className="title">{title}</div>
      <OverlayTrigger
        trigger={['hover', 'focus']}
        placement="left"
        overlay={CoursePopover}
        delay={100}>
        <InfoCircle className="info-circle" />
      </OverlayTrigger>
      {invalid && <OverlayTrigger
        trigger={['hover', 'focus']}
        placement="right"
        overlay={WarningPopover}
        delay={100}>
        <ExclamationTriangle />
      </OverlayTrigger>}
    </div>
  );
};

export default Course;
