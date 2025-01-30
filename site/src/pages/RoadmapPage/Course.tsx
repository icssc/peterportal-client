import React, { FC, useState } from 'react';
import './Course.scss';
import { Button } from 'react-bootstrap';
import { ExclamationTriangle, Trash, BagPlus, BagFill } from 'react-bootstrap-icons';
import CourseQuarterIndicator from '../../component/QuarterTooltip/CourseQuarterIndicator';
import CoursePopover from '../../component/CoursePopover/CoursePopover';
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import Popover from 'react-bootstrap/Popover';
import { useIsMobile } from '../../helpers/util';

import { CourseGQLData } from '../../types/types';
import ThemeContext from '../../style/theme-context';
import { setActiveCourse, setShowAddCourse } from '../../store/slices/roadmapSlice';
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
  data: CourseGQLData | string;
  popupListener?: (open: boolean) => void;
  openPopoverLeft?: boolean;
  requiredCourses?: string[];
  /** Whether to always collapse whitespace in the course name */
  alwaysCollapse?: boolean;
}
export const CourseNameAndInfo: React.FC<CourseNameAndInfoProps> = (props) => {
  const { data, openPopoverLeft, requiredCourses, popupListener, alwaysCollapse } = props;
  const { id, department, courseNumber } =
    typeof data === 'string' ? { id: data, department: data, courseNumber: '' } : data;

  const [showInfoPopover, setShowInfoPopover] = useState(false);
  const [allowTouchClick, setAllowTouchClick] = useState(false);
  const [showPopoverTimeout, setShowPopoverTimeout] = useState(0);
  const courseRoute = '/course/' + department.replace(/\s+/g, '') + courseNumber.replace(/\s+/g, '');
  const showSearch = useAppSelector((state) => state.roadmap.showSearch);
  const isMobile = useIsMobile();
  let courseID = department + ' ' + courseNumber;
  if (alwaysCollapse) courseID = courseID.replace(/\s/g, '');

  const POPOVER_DELAY = 80;
  const TOUCH_DELAY = 120;

  const showPopover = () => {
    setShowInfoPopover(true);
    popupListener?.(true);
    clearTimeout(showPopoverTimeout);
    setShowPopoverTimeout(0);
    setTimeout(() => setAllowTouchClick(true), TOUCH_DELAY);
  };
  const hidePopover = () => {
    setShowInfoPopover(false);
    setAllowTouchClick(false);
    popupListener?.(false);
    clearTimeout(showPopoverTimeout);
    setShowPopoverTimeout(0);
  };

  const handleMouseMove = () => {
    if (!showPopoverTimeout) return;
    clearTimeout(showPopoverTimeout);
    setShowPopoverTimeout(window.setTimeout(showPopover, POPOVER_DELAY));
  };

  const handleHoverTitle = () => {
    if (document.querySelector('.course.sortable-fallback')) return;
    if (isMobile && showSearch) return;
    clearTimeout(showPopoverTimeout);
    setShowPopoverTimeout(window.setTimeout(showPopover, POPOVER_DELAY));
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
      <span onMouseEnter={handleHoverTitle} onMouseLeave={handleUnhoverTitle} onMouseMove={handleMouseMove}>
        <a className="name" href={courseRoute} target="_blank" rel="noopener noreferrer" onClick={handleLinkClick}>
          {courseID}
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
  unmatchedPrerequisites?: string[];
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
            {minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`} unit{maxUnits === 1 ? '' : 's'}
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
      </div>
    </div>
  );
};

export default Course;
