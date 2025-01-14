import React, { FC } from 'react';
import './Course.scss';
import { Button } from 'react-bootstrap';
import { InfoCircle, ExclamationTriangle, Trash, BagPlus, BagFill } from 'react-bootstrap-icons';
import CourseQuarterIndicator from '../../component/QuarterTooltip/CourseQuarterIndicator';
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import Popover from 'react-bootstrap/Popover';

import { CourseGQLData } from '../../types/types';
import ThemeContext from '../../style/theme-context';
import { setActiveCourse, setShowAddCourse } from '../../store/slices/roadmapSlice';
import { useAppDispatch } from '../../store/hooks';

export const UnmetPrerequisiteText: React.FC<{ requiredCourses?: string[] }> = ({ requiredCourses }) => (
  <>
    Prerequisite(s) not met! Missing: {requiredCourses?.join(', ')}
    <br />
    Already completed prerequisite(s) at another institution? Click 'Transfer Credits' at the top of the planner to
    clear the prerequisite(s).
  </>
);

interface CourseProps extends CourseGQLData {
  requiredCourses?: string[];
  unmatchedPrerequisites?: string[];
  onDelete?: () => void;
  onAddToBag?: () => void;
  isInBag?: boolean;
  removeFromBag?: () => void;
  addMode?: 'tap' | 'drag';
}

const Course: FC<CourseProps> = (props) => {
  const {
    id,
    department,
    courseNumber,
    title,
    minUnits,
    maxUnits,
    description,
    prerequisiteText,
    corequisites,
    requiredCourses,
    terms,
    onDelete,
    onAddToBag,
    isInBag,
    removeFromBag,
  } = props;
  const CoursePopover = (
    <Popover id={'course-popover-' + id}>
      <Popover.Content>
        <div className="course-popover">
          <div className="popover-name">
            {department + ' ' + courseNumber} {title}
          </div>
          <div className="popover-units">
            <span className="popover-units-value">{minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`}</span>{' '}
            units
          </div>
          <div className="popover-description">{description}</div>
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
        </div>
      </Popover.Content>
    </Popover>
  );

  const dispatch = useAppDispatch();

  const warningPopover = (
    <Popover id={'warning-popover-' + id}>
      <Popover.Content>
        <UnmetPrerequisiteText requiredCourses={requiredCourses} />
      </Popover.Content>
    </Popover>
  );

  const courseRoute = '/course/' + props.department.replace(/\s+/g, '') + props.courseNumber.replace(/\s+/g, '');

  const insertCourseOnClick = () => {
    dispatch(setActiveCourse(props));
    dispatch(setShowAddCourse(true));
  };

  const tapProps = { onClick: insertCourseOnClick, role: 'button', tabIndex: 0 };
  const tappableCourseProps = props.addMode === 'tap' ? tapProps : {};

  return (
    <div className={`course ${requiredCourses ? 'invalid' : ''}`} {...tappableCourseProps}>
      <div className="course-card-top">
        <div className="course-and-info">
          <a className="name" href={courseRoute} target="_blank" rel="noopener noreferrer">
            {department + ' ' + courseNumber}
          </a>
          <span className="units">
            {minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`} unit{maxUnits === 1 ? '' : 's'}
          </span>
          <OverlayTrigger trigger={['hover', 'focus']} placement="auto" overlay={CoursePopover} delay={100}>
            <InfoCircle />
          </OverlayTrigger>
          {requiredCourses && (
            <OverlayTrigger trigger={['hover', 'focus']} placement="right" overlay={warningPopover} delay={100}>
              <ExclamationTriangle />
            </OverlayTrigger>
          )}
        </div>
        <div className="spacer"></div>
        {onDelete ? (
          <ThemeContext.Consumer>
            {({ darkMode }) => (
              <Button
                variant={darkMode ? 'dark' : 'light'}
                className="course-delete-btn"
                onClick={onDelete}
                aria-label="delete"
              >
                <Trash className="course-delete-icon" />
              </Button>
            )}
          </ThemeContext.Consumer>
        ) : (
          <CourseQuarterIndicator terms={terms} size="xs" />
        )}
      </div>
      <div className="title">{title}</div>
      <div className="course-footer">
        {onAddToBag && !isInBag && <BagPlus onClick={onAddToBag}></BagPlus>}
        {isInBag && <BagFill onClick={removeFromBag}></BagFill>}
        {/* <div className="course-footer">
        {requiredCourses && (
          <OverlayTrigger trigger={['hover', 'focus']} placement="right" overlay={WarningPopover} delay={100}>
            <ExclamationTriangle />
          </OverlayTrigger>
        )}
        {/* <div className="units">{minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`} units</div> * /}
      </div> */}
      </div>
    </div>
  );
};

export default Course;
