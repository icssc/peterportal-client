import React, { FC } from "react";
import "./Course.scss";
import { InfoCircle } from "react-bootstrap-icons";
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import Popover from 'react-bootstrap/Popover';

import { CourseData } from '../../types/types';

interface CourseProps extends CourseData {
}

const Course: FC<CourseProps> = (props) => {
  let { id, department, number, title, units, description, prerequisite_text } = props;

  const HoverPopover = <Popover id={'course-popover-' + id}>
    <Popover.Content>
      <div className="course-popover">
        <div className="popover-name">{department + ' ' + number} {title}</div>
        <div className="popover-units">
          <span className="popover-units-value">{units[0]}</span> units
        </div>
        <div className="popover-description">{description}</div>
        <div className="popover-prerequisites">
          <span className="popover-prerequisites-prefix">Prequisite:</span> {prerequisite_text}
        </div>
      </div>
    </Popover.Content>
  </Popover>

  return (
    <div className="course">
      <div className="course-card-top">
        <div className="name">{department + ' ' + number}</div>
        <div className="units">{units[0]} units</div>
      </div>

      <div className="title">{title}</div>
      <OverlayTrigger
        trigger={['hover', 'focus']}
        placement="left"
        overlay={HoverPopover}
        delay={100}>
        <InfoCircle className="info-circle" />
      </OverlayTrigger>
    </div>
  );
};

export default Course;
