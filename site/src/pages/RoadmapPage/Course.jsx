import React from "react";
import "./Course.scss";
import {InfoCircle} from "react-bootstrap-icons";
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import Popover from 'react-bootstrap/Popover';

const HoverPopover = ( {name, title, units, description, prerequisite_text} ) => {
  return (
    <Popover>
      <Popover.Content>
        <div className="course-popover"> 
          <div className="popover-name">{name} {title}</div>
          <div className="popover-units">
            <span className="popover-units-value">{units}</span> units
          </div>
          <div className="popover-description">{description}</div>
          <div className="popover-prerequisites">
            <span className="popover-prerequisites-prefix">Prequisite:</span> {prerequisite_text}
          </div>
        </div>
      </Popover.Content>
    </Popover>
  );
};

const Course = ({ name, title, units, description, prerequisite_text }) => {
  return (
      <div className="course">
        <div className="course-card-top">
          <div className="name">{name}</div>
          <div className="units">{units} units</div>
        </div>

        <div className="title">{title}</div>
          <OverlayTrigger 
            trigger="hover" 
            placement="left"
            overlay={HoverPopover({name, title, units, description, prerequisite_text})}
            delay={100}>
            <InfoCircle className="info-circle"/>
          </OverlayTrigger>
      </div>
    
  );
};

export default Course;
