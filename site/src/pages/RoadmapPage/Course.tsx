import React, { FC, useState } from 'react';
import './Course.scss';
import { Button } from 'react-bootstrap';
import CourseQuarterIndicator from '../../component/QuarterTooltip/CourseQuarterIndicator';
import CoursePopover from '../../component/CoursePopover/CoursePopover';
import { useIsMobile, pluralize } from '../../helpers/util';

import { CourseGQLData } from '../../types/types';
import ThemeContext from '../../style/theme-context';
import { setActiveCourse, setShowAddCourse, setActiveMissingPrerequisites } from '../../store/slices/roadmapSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import PPCOverlayTrigger from '../../component/PPCOverlayTrigger';

import { IconButton } from '@mui/material';
import WarningAmberIcon from '@mui/icons-material/WarningAmber';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import AddShoppingCartIcon from '@mui/icons-material/AddShoppingCart';
import ShoppingCartIcon from '@mui/icons-material/ShoppingCart';

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
  const { department, courseNumber } = typeof data === 'string' ? { department: data, courseNumber: '' } : data;

  const [allowTouchClick, setAllowTouchClick] = useState(false);
  const showSearch = useAppSelector((state) => state.roadmap.showSearch);
  const isMobile = useIsMobile();

  const courseRoute = '/course/' + department.replace(/\s+/g, '') + courseNumber.replace(/\s+/g, '');
  let courseID = department + ' ' + courseNumber;
  if (alwaysCollapse) courseID = courseID.replace(/\s/g, '');

  const handleLinkClick = (event: React.MouseEvent) => {
    const isTouchEvent = !(event.target as HTMLAnchorElement).matches(':focus');
    if (isTouchEvent && !allowTouchClick) event.preventDefault();
  };

  const popoverContent = <CoursePopover course={data} interactive={true} requiredCourses={requiredCourses} />;

  return (
    <PPCOverlayTrigger
      popoverContent={popoverContent}
      placement={isMobile ? 'bottom' : openPopoverLeft ? 'left-start' : 'right-start'}
      popupListener={popupListener}
      setAllowSecondaryTap={setAllowTouchClick}
      disabled={isMobile && showSearch}
    >
      <span>
        <a className="name" href={courseRoute} target="_blank" rel="noopener noreferrer" onClick={handleLinkClick}>
          {courseID}
        </a>
        {requiredCourses && (
          <span className="warning-container">
            <WarningAmberIcon />
          </span>
        )}
      </span>
    </PPCOverlayTrigger>
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
    <div className="course" {...tappableCourseProps}>
      <div className="course-card-top">
        <div className="course-and-info">
          <span className={`${requiredCourses ? 'missing-prereq' : ''}`}>
            <CourseNameAndInfo data={props.data} {...{ openPopoverLeft, requiredCourses }} />
          </span>
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
                <DeleteOutlineIcon className="course-delete-icon" />
              </Button>
            )}
          </ThemeContext.Consumer>
        ) : (
          <CourseQuarterIndicator terms={terms} size="xs" />
        )}
      </div>
      <div className="title">{title}</div>
      <div className="course-footer">
        {onAddToBag && !isInBag && (
          <IconButton onClick={onAddToBag}>
            <AddShoppingCartIcon />
          </IconButton>
        )}
        {isInBag && (
          <IconButton onClick={removeFromBag}>
            <ShoppingCartIcon />
          </IconButton>
        )}
      </div>
    </div>
  );
};

export default Course;
