import { FC, useState } from 'react';
import './Course.scss';
import CourseQuarterIndicator from '../../component/QuarterTooltip/CourseQuarterIndicator';
import CoursePopover from '../../component/CoursePopover/CoursePopover';
import { useIsMobile, getUnitText } from '../../helpers/util';

import { CourseGQLData } from '../../types/types';
import { setActiveCourse, setShowAddCourse, setActiveMissingPrerequisites } from '../../store/slices/roadmapSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import PPCOverlayTrigger from '../../component/PPCOverlayTrigger';

import { IconButton } from '@mui/material';
import WarningAmberIcon from '@mui/icons-material/WarningAmber';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';

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

  const popoverContent = <CoursePopover course={data} requiredCourses={requiredCourses} />;

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
            <WarningAmberIcon className="course-warn-icon" />
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
    <div className="course" {...tappableCourseProps}>
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
