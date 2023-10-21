import React, { FC } from "react";
import "./Course.scss";
import { Button } from "react-bootstrap";
import { InfoCircle, ExclamationTriangle, Trash } from "react-bootstrap-icons";
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import Popover from 'react-bootstrap/Popover';

import { CourseGQLData } from '../../types/types';
import ThemeContext from "src/style/theme-context";

interface CourseProps extends CourseGQLData {
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
          <span className="popover-detail-prefix">Prerequisite:</span> {prerequisite_text}
        </div>}
        {corequisite && <div className="popover-detail">
          <span className="popover-detail-prefix">Corequisite:</span> {corequisite}
        </div>}
      </div>
    </Popover.Content>
  </Popover>

  const WarningPopover = <Popover id={'warning-popover-' + id}>
    <Popover.Content>
      Prerequisite(s) not met! Missing: {requiredCourses?.join(', ')} 
      <br />
      Already completed prerequisite(s) at another institution? Click 'Transfer Credits' at the top of the planner to clear the prerequisite(s).
    </Popover.Content>
  </Popover>

  const courseRoute = () => {
    return '/course/' + props.department.replace(/\s+/g, '') + props.number.replace(/\s+/g, '')
  }

  return (
    <div className={`course ${requiredCourses ? 'invalid' : ''}`}>
      <div className="course-card-top">
        <div className="course-and-info">
          <a className="name" href={courseRoute()} target="_blank" rel="noopener noreferrer">{department + ' ' + number}</a>
          <OverlayTrigger
            trigger={['hover', 'focus']}
            placement="auto"
            overlay={CoursePopover}
            delay={100}>
            <InfoCircle className="info-circle" />
          </OverlayTrigger>
        </div>
        {onDelete && (
          <ThemeContext.Consumer>
            {({ darkMode }) =>
              <Button variant={darkMode ? 'dark' : 'light'} className="course-delete-btn" onClick={onDelete}>
                <Trash className="course-delete-icon" />
              </Button>
            }
          </ThemeContext.Consumer>
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
