'use client';
import { FC } from 'react';
import Modal from 'react-bootstrap/Modal';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { moveCourse, setShowAddCourse, setShowSearch } from '../../../store/slices/roadmapSlice';
import './AddCoursePopup.scss';
import UIOverlay from '../../../component/UIOverlay/UIOverlay';
import { useNamedAcademicTerm } from '../../../hooks/namedAcademicTerm';
import { pluralize } from '../../../helpers/util';
import {
  CourseBookmarkButton,
  CourseDescription,
  IncompletePrerequisiteText,
  PrerequisiteText,
  PreviousOfferingsRow,
} from '../../../component/CourseInfo/CourseInfo';

import { IconButton } from '@mui/material';
import CloseIcon from '@mui/icons-material/Close';

const AddCoursePopup: FC = () => {
  const currentYearAndQuarter = useAppSelector((state) => state.roadmap.currentYearAndQuarter);
  const showAddCourse = useAppSelector((state) => state.roadmap.showAddCourse);
  const activeCourse = useAppSelector((state) => state.roadmap.activeCourse);
  const activeMissingPrerequisites = useAppSelector((state) => state.roadmap.activeMissingPrerequisites);
  const term = useNamedAcademicTerm();

  const dispatch = useAppDispatch();

  const quarter = currentYearAndQuarter?.quarter ?? -1;
  const year = currentYearAndQuarter?.year ?? -1;

  const closePopup = () => dispatch(setShowAddCourse(false));
  const contentClassName = 'ppc-modal add-course-modal ' + (showAddCourse ? 'enter' : 'exit');
  const overlay = <UIOverlay onClick={closePopup} zIndex={499} />;

  const addToRoadmap = () => {
    dispatch(
      moveCourse({
        from: { yearIndex: -1, quarterIndex: -1, courseIndex: -1 },
        to: { yearIndex: year, quarterIndex: quarter, courseIndex: 0 },
      }),
    );

    // hide the search bar to view the roadmap
    dispatch(setShowSearch({ show: false }));

    closePopup();
  };

  if (!activeCourse)
    return (
      <>
        <div className={contentClassName}></div>
        {overlay}
      </>
    );

  const { minUnits, maxUnits, department, courseNumber } = activeCourse;

  return (
    <>
      <div className={contentClassName}>
        <Modal.Header>
          <h2>
            {department} {courseNumber}
          </h2>
          <span className="unit-count">
            ({minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`} unit{pluralize(maxUnits)})
          </span>
          <CourseBookmarkButton course={activeCourse} />
          <div className="spacer"></div>
          <IconButton onClick={closePopup} className="close-button">
            <CloseIcon />
          </IconButton>
        </Modal.Header>
        <Modal.Body>
          <CourseDescription course={activeCourse} />
          {activeMissingPrerequisites ? (
            <IncompletePrerequisiteText requiredCourses={activeMissingPrerequisites} />
          ) : (
            <PrerequisiteText course={activeCourse} />
          )}
          <PreviousOfferingsRow course={activeCourse} />
        </Modal.Body>
        <button className="fixed" onClick={addToRoadmap}>
          Add to {term.quarter} {term.year}
        </button>
      </div>
      {overlay}
    </>
  );
};

export default AddCoursePopup;
