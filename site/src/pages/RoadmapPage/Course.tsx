import React, { FC, useState } from 'react';
import './Course.scss';
import { Button } from 'react-bootstrap';
import { ExclamationTriangle, Trash, BagPlus, BagFill } from 'react-bootstrap-icons';
import CourseQuarterIndicator from '../../component/QuarterTooltip/CourseQuarterIndicator';
import CoursePopover from '../../component/CoursePopover/CoursePopover';
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import Popover from 'react-bootstrap/Popover';
import { useIsMobile, pluralize } from '../../helpers/util';

import { CourseGQLData } from '../../types/types';
import ThemeContext from '../../style/theme-context';
import { setActiveCourse, setShowAddCourse, setActiveMissingPrerequisites } from '../../store/slices/roadmapSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';

export const UnmetPrerequisiteText: React.FC<{ requiredCourses?: string[] }> = ({ requiredCourses }) => (
  <>
    Prerequisite(s) not met! Missing: {requiredCourses?.join(', ')}
    <br />
    Already completed prerequisite(s) at another institution? Click 'Transfer Credits' at the top of the planner to
    clear the prerequisite(s).
  </>
);

interface CourseNameAndInfoProps {
  data: CourseGQLData;
  openPopoverLeft?: boolean;
  requiredCourses?: string[];
}
const CourseNameAndInfo: React.FC<CourseNameAndInfoProps> = ({ data, openPopoverLeft, requiredCourses }) => {
  const { id, department, courseNumber } = data;

  const [showInfoPopover, setShowInfoPopover] = useState(false);
  const [allowTouchClick, setAllowTouchClick] = useState(false);
  const courseRoute = '/course/' + department.replace(/\s+/g, '') + courseNumber.replace(/\s+/g, '');
  const showSearch = useAppSelector((state) => state.roadmap.showSearch);
  const isMobile = useIsMobile();

  const showPopover = () => setShowInfoPopover(true);
  const hidePopover = () => {
    setShowInfoPopover(false);
    setAllowTouchClick(false);
  };

  const handleHoverTitle = () => {
    if (document.querySelector('.course.sortable-fallback')) return;
    if (isMobile && showSearch) return;
    showPopover();
    setTimeout(() => setAllowTouchClick(true), 100);
  };
  const handleUnhoverTitle = (event: React.MouseEvent) => {
    try {
      const inTooltip = document.querySelector('.ppc-popover')?.contains(event?.relatedTarget as HTMLElement);
      if (!inTooltip) hidePopover();
    } catch {
      hidePopover();
    }
  };

  const handleLinkClick = (event: React.MouseEvent) => {
    const isTouchEvent = !(event.target as HTMLAnchorElement).matches(':focus');
    if (isTouchEvent && !allowTouchClick) event.preventDefault();
  };

  const popover = (
    <Popover className="ppc-popover" id={'course-popover-' + id} onMouseLeave={hidePopover}>
      <CoursePopover course={data} interactive={true} requiredCourses={requiredCourses} />
    </Popover>
  );

  return (
    <OverlayTrigger
      show={showInfoPopover}
      placement={isMobile ? 'bottom' : openPopoverLeft ? 'left-start' : 'right-start'}
      overlay={popover}
    >
      <span onMouseEnter={handleHoverTitle} onMouseLeave={handleUnhoverTitle}>
        <a className="name" href={courseRoute} target="_blank" rel="noopener noreferrer" onClick={handleLinkClick}>
          {department + ' ' + courseNumber}
        </a>
        {requiredCourses && (
          <span className="warning-container">
            <ExclamationTriangle />
          </span>
        )}
      </span>
    </OverlayTrigger>
  );
};

interface CourseProps {
  requiredCourses?: string[];
  onDelete?: () => void;
  onAddToBag?: () => void;
  isInBag?: boolean;
  removeFromBag?: () => void;
  openPopoverLeft?: boolean;
  addMode?: 'tap' | 'drag';
  data: CourseGQLData;
}

const Course: FC<CourseProps> = (props) => {
  const { title, minUnits, maxUnits, terms } = props.data;
  const { requiredCourses, onDelete, onAddToBag, isInBag, removeFromBag, openPopoverLeft } = props;

  const dispatch = useAppDispatch();

  const insertCourseOnClick = () => {
    dispatch(setActiveCourse(props.data));
    dispatch(setActiveMissingPrerequisites(requiredCourses));
    dispatch(setShowAddCourse(true));
  };

  const tapProps = { onClick: insertCourseOnClick, role: 'button', tabIndex: 0 };
  const tappableCourseProps = props.addMode === 'tap' ? tapProps : {};

  return (
    <div className={`course ${requiredCourses ? 'invalid' : ''}`} {...tappableCourseProps}>
      <div className="course-card-top">
        <div className="course-and-info">
          <CourseNameAndInfo data={props.data} {...{ openPopoverLeft, requiredCourses }} />
          <span className="units">
            {minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`} unit{pluralize(maxUnits)}
          </span>
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
