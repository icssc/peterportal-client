import React, { FC, useState } from 'react';
import './Course.scss';
import { Button } from 'react-bootstrap';
import { InfoCircle, ExclamationTriangle, Trash, BagPlus, BagFill } from 'react-bootstrap-icons';
import CourseQuarterIndicator from '../../component/QuarterTooltip/CourseQuarterIndicator';
import CoursePopover from '../../component/CoursePopover/CoursePopover';
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import Popover from 'react-bootstrap/Popover';
import { useIsMobile } from '../../helpers/util';

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
  const { id, department, courseNumber, title, minUnits, maxUnits, terms } = props.data;
  const { requiredCourses, onDelete, onAddToBag, isInBag, removeFromBag, openPopoverLeft } = props;

  const dispatch = useAppDispatch();

  const [showInfoPopover, setShowInfoPopover] = useState(false);
  const courseRoute = '/course/' + department.replace(/\s+/g, '') + courseNumber.replace(/\s+/g, '');
  const isMobile = useIsMobile();

  const insertCourseOnClick = () => {
    dispatch(setActiveCourse(props.data));
    dispatch(setShowAddCourse(true));
  };

  const showPopover = () => setShowInfoPopover(true);
  const hidePopover = () => setShowInfoPopover(false);

  const createPopoverListeners = (requiresDeletePresent: boolean) => ({
    onMouseEnter: () => !!onDelete === requiresDeletePresent && showPopover(),
    onMouseLeave: (event: React.MouseEvent) => {
      if (!!onDelete !== requiresDeletePresent) return;
      const inTooltip = document.querySelector('.ppc-popover')?.contains(event?.relatedTarget as HTMLElement);
      if (!inTooltip) setShowInfoPopover(false);
    },
  });

  const tapProps = { onClick: insertCourseOnClick, role: 'button', tabIndex: 0 };
  const tappableCourseProps = props.addMode === 'tap' ? tapProps : {};

  const popover = (
    <Popover className="ppc-popover" id={'course-popover-' + id} onMouseLeave={hidePopover}>
      <CoursePopover course={props.data} interactive={true} />
    </Popover>
  );

  return (
    <div className={`course ${requiredCourses ? 'invalid' : ''}`} {...tappableCourseProps}>
      <div className="course-card-top">
        <div className="course-and-info">
          <OverlayTrigger
            show={!onDelete && showInfoPopover && !isMobile}
            placement={isMobile ? 'bottom' : openPopoverLeft ? 'left-start' : 'right-start'}
            overlay={popover}
          >
            <a
              className="name"
              href={courseRoute}
              target="_blank"
              rel="noopener noreferrer"
              {...createPopoverListeners(false)}
            >
              {department + ' ' + courseNumber}
            </a>
          </OverlayTrigger>
          <span className="units">
            {minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`} unit{maxUnits === 1 ? '' : 's'}
          </span>
          <OverlayTrigger
            show={onDelete && showInfoPopover}
            placement={isMobile ? 'bottom' : openPopoverLeft ? 'left-start' : 'right-start'}
            overlay={popover}
          >
            <div className="info-wrapper" {...createPopoverListeners(true)}>
              {requiredCourses ? <ExclamationTriangle /> : <InfoCircle />}
            </div>
          </OverlayTrigger>
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
