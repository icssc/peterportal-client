import React, { FC, useState } from 'react';
import './Course.scss';

import RecentOfferingsTooltip from '../../../component/RecentOfferingsTooltip/RecentOfferingsTooltip';
import CoursePopover from '../../../component/CoursePopover/CoursePopover';
import OverlayTrigger from '../../../component/OverlayTrigger/OverlayTrigger';

import { useIsMobile, pluralize, formatGEsTag, shortenCourseLevel } from '../../../helpers/util';
import { CourseGQLData, PlannerCourseData } from '../../../types/types';
import { setActiveCourse, setShowAddCourse, setActiveMissingPrerequisites } from '../../../store/slices/roadmapSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';

import { IconButton, TextField } from '@mui/material';
import WarningAmberIcon from '@mui/icons-material/WarningAmber';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import DragIndicatorIcon from '@mui/icons-material/DragIndicator';
import { addPreview, clearPreviews } from '../../../store/slices/previewSlice';
import { CourseBookmarkButton, CourseSynopsis } from '../../../component/CourseInfo/CourseInfo';
import Link from 'next/link';

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
  const showSearch = useAppSelector((state) => state.roadmap.showMobileCatalog);
  const isMobile = useIsMobile();

  const encodedCourseTitle = encodeURIComponent(department.replace(/\s+/g, '') + courseNumber.replace(/\s+/g, ''));
  const courseRoute = '/course/' + encodedCourseTitle;
  let courseID = department + ' ' + courseNumber;
  if (alwaysCollapse) courseID = courseID.replace(/\s/g, '');

  const handleLinkClick = (event: React.MouseEvent) => {
    event.preventDefault();
    if (isMobile && showSearch) return;
    dispatch(clearPreviews());
    dispatch(addPreview({ type: 'course', id: courseID }));
  };

  const popoverContent = <CoursePopover course={data} requiredCourses={requiredCourses} />;

  return (
    <OverlayTrigger
      popoverContent={popoverContent}
      popupListener={popupListener}
      disabled={isMobile}
      anchor={openPopoverLeft ? 'left' : 'right'}
      transform={openPopoverLeft ? 'left' : 'right'}
    >
      <span>
        <Link className="name" href={courseRoute} target="_blank" rel="noopener noreferrer" onClick={handleLinkClick}>
          {courseID}
        </Link>
        {requiredCourses && (
          <span className="warning-container">
            <WarningAmberIcon />
          </span>
        )}
      </span>
    </OverlayTrigger>
  );
};

interface CourseProps {
  requiredCourses?: string[];
  onDelete?: () => void;
  openPopoverLeft?: boolean;
  addMode?: 'tap' | 'drag';
  data: PlannerCourseData;
}

const Course: FC<CourseProps> = (props) => {
  const { title, courseLevel, minUnits, maxUnits, terms, geList } = props.data;
  const { requiredCourses, onDelete, openPopoverLeft } = props;

  const isInRoadmap = !!onDelete;
  const isMobile = useIsMobile();

  const formattedCourseLevel = shortenCourseLevel(courseLevel);
  const geTags = formatGEsTag(geList);

  const dispatch = useAppDispatch();

  const insertCourseOnClick = () => {
    dispatch(setActiveCourse({ course: props.data }));
    dispatch(setActiveMissingPrerequisites(requiredCourses));
    dispatch(setShowAddCourse(true));
  };

  const tapProps = { onClick: insertCourseOnClick, role: 'button', tabIndex: 0 };
  const tappableCourseProps = props.addMode === 'tap' ? tapProps : {};

  /**
   * @todo merge conflict with variable units - when merging with var units, this
   * text should be used in course tags, but not in the course-card-top in the Roadmap
   */
  const unitsText = `${minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`} unit${pluralize(maxUnits)}`;
  const [inputUnit, setInputUnit] = useState('');

  const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setInputUnit(e.target.value);
    console.log(inputUnit);
  };

  const keyPress = (event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key == 'Enter') {
      event.preventDefault();
      console.log(inputUnit);
      // update something
    }
  };

  return (
    <div className={`course ${isInRoadmap ? 'roadmap-course' : ''}`} {...tappableCourseProps}>
      {(!isMobile || isInRoadmap) && (
        <div className="course-drag-handle">
          <DragIndicatorIcon />
        </div>
      )}

      <div className="course-card-top">
        <div className="course-and-info">
          <span className={`${requiredCourses ? 'missing-prereq' : ''}`}>
            <CourseNameAndInfo data={props.data} {...{ openPopoverLeft, requiredCourses }} />
          </span>
          {isInRoadmap && minUnits === maxUnits && <span className="units">{unitsText}</span>}
          {
            // @todo have better unit submission
            //       update UI after submission
            //       replace "units" text placeholder
            isInRoadmap && minUnits !== maxUnits && (
              <div className="custom-units">
                <TextField
                  className="unit-input"
                  defaultValue={2}
                  size="small"
                  fullWidth={false}
                  onChange={handleChange}
                  onKeyDown={keyPress}
                />
                <span className="units">{'units'}</span>
              </div>
            )
          }
        </div>
        {isInRoadmap ? (
          <IconButton className="course-delete-btn" onClick={onDelete} aria-label="delete">
            <DeleteOutlineIcon className="course-delete-icon" />
          </IconButton>
        ) : (
          <CourseBookmarkButton course={props.data} />
        )}
      </div>
      {isInRoadmap ? (
        <div className="title">{title}</div>
      ) : (
        <div className="course-info">
          <CourseSynopsis course={props.data} clampDescription={3} />
          <div className="course-tags">
            {`${unitsText} • ${formattedCourseLevel} • ${geTags.length > 0 ? geTags + ' • ' : ''}`}
            <RecentOfferingsTooltip terms={terms} />
          </div>
        </div>
      )}
    </div>
  );
};

export default Course;
