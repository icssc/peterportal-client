import { FC, useState } from 'react';
import './Course.scss';
import CourseQuarterIndicator from '../../component/QuarterTooltip/CourseQuarterIndicator';
import CoursePopover from '../../component/CoursePopover/CoursePopover';
import { useIsMobile, getUnitText, removeWhitespace } from '../../helpers/util';

import { CourseGQLData } from '../../types/types';
import { setActiveCourse, setShowAddCourse, setActiveMissingPrerequisites } from '../../store/slices/roadmapSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import PPCOverlayTrigger from '../../component/PPCOverlayTrigger/PPCOverlayTrigger';

import { IconButton } from '@mui/material';
import WarningAmberIcon from '@mui/icons-material/WarningAmber';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import DragIndicatorIcon from '@mui/icons-material/DragIndicator';

interface CourseNameAndInfoProps {
  data: CourseGQLData | string;
  popupListener?: (open: boolean) => void;
  openPopoverLeft?: boolean;
  requiredCourses?: string[];
  /** Whether to always collapse whitespace in the course name */
  alwaysCollapse?: boolean;
}
export const CourseNameAndInfo: FC<CourseNameAndInfoProps> = (props) => {
  const { data, popupListener, openPopoverLeft, requiredCourses, alwaysCollapse } = props;

  const courseRoute = removeWhitespace(
    typeof data === 'string' ? `/course/${data}` : `/course/${data.department}${data.courseNumber}`,
  );

  let courseID = typeof data === 'string' ? data : `${data.department} ${data.courseNumber}`;
  if (alwaysCollapse) courseID = removeWhitespace(courseID);

  const [allowTouchClick, setAllowTouchClick] = useState(false);
  const showSearch = useAppSelector((state) => state.roadmap.showSearch);
  const isMobile = useIsMobile();

  const handleLinkClick = (event: React.MouseEvent) => {
    const isTouchEvent = !(event.target as HTMLAnchorElement).matches(':focus');
    if (isTouchEvent && !allowTouchClick) event.preventDefault();
  };

  const popoverContent = <CoursePopover course={data} />;

  return (
    <PPCOverlayTrigger
      popoverContent={popoverContent}
      placement={isMobile ? 'bottom' : openPopoverLeft ? 'left-start' : 'right-start'}
      popupListener={popupListener}
      setAllowSecondaryTap={setAllowTouchClick}
      disabled={isMobile && showSearch}
    >
      <>
        <a className="name" href={courseRoute} target="_blank" rel="noopener noreferrer" onClick={handleLinkClick}>
          {courseID}
        </a>
        {requiredCourses && (
          <span className="warning-container">
            <WarningAmberIcon />
          </span>
        )}
      </>
    </PPCOverlayTrigger>
  );
};

interface CourseProps {
  course: CourseGQLData;
  onDelete?: () => void;
  requiredCourses?: string[];
  openPopoverLeft?: boolean;
  addMode?: 'tap' | 'drag';
}

const Course: FC<CourseProps> = ({ course, requiredCourses, onDelete, openPopoverLeft, addMode }) => {
  const dispatch = useAppDispatch();

  const insertCourseOnClick = () => {
    dispatch(setActiveCourse(course));
    dispatch(setActiveMissingPrerequisites(requiredCourses));
    dispatch(setShowAddCourse(true));
  };

  const tapProps = { onClick: insertCourseOnClick, role: 'button', tabIndex: 0 };
  const tappableCourseProps = addMode === 'tap' ? tapProps : {};
  const unitText = getUnitText(course);

  return (
    <div className={`course ${onDelete ? 'roadmap-course' : ''}`} {...tappableCourseProps}>
      {onDelete && (
        <div className="course-drag-handle">
          <DragIndicatorIcon />
        </div>
      )}
      <div className="course-card-top">
        <div className="course-and-info">
          <span className={`${requiredCourses ? 'missing-prereq' : ''}`}>
            <CourseNameAndInfo data={course} openPopoverLeft={openPopoverLeft} requiredCourses={requiredCourses} />
          </span>
          <span className="units">{unitText}</span>
        </div>
        {onDelete ? (
          <IconButton className="course-delete-btn" onClick={onDelete} aria-label="delete">
            <DeleteOutlineIcon className="course-delete-icon" />
          </IconButton>
        ) : (
          <CourseQuarterIndicator terms={course.terms} size="xs" />
        )}
      </div>
      <div className="title">{course.title}</div>
    </div>
  );
};

export default Course;
