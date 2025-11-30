import React, { FC } from 'react';
import './Course.scss';

import RecentOfferingsTooltip from '../../../component/RecentOfferingsTooltip/RecentOfferingsTooltip';
import CoursePopover from '../../../component/CoursePopover/CoursePopover';
import PPCOverlay from '../../../component/PPCOverlayTrigger/PPCOverlay';

import { useIsMobile, pluralize } from '../../../helpers/util';
import { CourseGQLData } from '../../../types/types';
import { setActiveCourse, setShowAddCourse, setActiveMissingPrerequisites } from '../../../store/slices/roadmapSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';

import { IconButton } from '@mui/material';
import WarningAmberIcon from '@mui/icons-material/WarningAmber';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import DragIndicatorIcon from '@mui/icons-material/DragIndicator';
import { setPreviewedCourse } from '../../../store/slices/coursePreviewSlice';

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

  const dispatch = useAppDispatch();
  const showSearch = useAppSelector((state) => state.roadmap.showSearch);
  const isMobile = useIsMobile();

  const courseRoute = '/course/' + department.replace(/\s+/g, '') + courseNumber.replace(/\s+/g, '');
  let courseID = department + ' ' + courseNumber;
  if (alwaysCollapse) courseID = courseID.replace(/\s/g, '');

  const handleLinkClick = (event: React.MouseEvent) => {
    event.preventDefault();
    if (isMobile && showSearch) return;
    dispatch(setPreviewedCourse(courseID));
  };

  const popoverContent = <CoursePopover course={data} requiredCourses={requiredCourses} />;

  return (
    <PPCOverlay
      popoverContent={popoverContent}
      popupListener={popupListener}
      disabled={isMobile}
      anchor={isMobile ? 'bottom' : openPopoverLeft ? 'left' : 'right'}
      transform={isMobile ? 'bottom' : openPopoverLeft ? 'left' : 'right'}
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
    </PPCOverlay>
  );
};

interface CourseProps {
  requiredCourses?: string[];
  onDelete?: () => void;
  openPopoverLeft?: boolean;
  addMode?: 'tap' | 'drag';
  data: CourseGQLData;
}

const Course: FC<CourseProps> = (props) => {
  const { title, minUnits, maxUnits, terms } = props.data;
  const { requiredCourses, onDelete, openPopoverLeft } = props;

  const dispatch = useAppDispatch();

  const insertCourseOnClick = () => {
    dispatch(setActiveCourse({ course: props.data }));
    dispatch(setActiveMissingPrerequisites(requiredCourses));
    dispatch(setShowAddCourse(true));
  };

  const tapProps = { onClick: insertCourseOnClick, role: 'button', tabIndex: 0 };
  const tappableCourseProps = props.addMode === 'tap' ? tapProps : {};

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
            <CourseNameAndInfo data={props.data} {...{ openPopoverLeft, requiredCourses }} />
          </span>
          <span className="units">
            {minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`} unit{pluralize(maxUnits)}
          </span>
        </div>
        {onDelete ? (
          <IconButton className="course-delete-btn" onClick={onDelete} aria-label="delete">
            <DeleteOutlineIcon className="course-delete-icon" />
          </IconButton>
        ) : (
          <div className="course-tooltip">
            <RecentOfferingsTooltip terms={terms} />
          </div>
        )}
      </div>
      <div className="title">{title}</div>
    </div>
  );
};

export default Course;
