import { FC } from 'react';
import Modal from 'react-bootstrap/Modal';
import './AddCoursePopup.scss';

import {
  CourseBookmarkButton,
  CourseDescription,
  IncompletePrerequisiteText,
  PrerequisiteText,
  PreviousOfferingsRow,
} from '../../component/CourseInfo/CourseInfo';
import UIOverlay from '../../component/UIOverlay/UIOverlay';

import { moveCourse, setShowAddCourse, setShowSearch } from '../../store/slices/roadmapSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { useNamedAcademicTerm } from '../../hooks/namedAcademicTerm';
import { pluralize } from '../../helpers/util';

import CloseIcon from '@mui/icons-material/Close';

const AddCoursePopup: FC = () => {
  const currentYearAndQuarter = useAppSelector((state) => state.roadmap.currentYearAndQuarter);
  const showAddCourse = useAppSelector((state) => state.roadmap.showAddCourse);
  const activeCourse = useAppSelector((state) => state.roadmap.activeCourse);
  const activeMissingPrerequisites = useAppSelector((state) => state.roadmap.activeMissingPrerequisites);
  const term = useNamedAcademicTerm();
  const dispatch = useAppDispatch();
  const closePopup = () => dispatch(setShowAddCourse(false));

  const quarter = currentYearAndQuarter?.quarter ?? -1;
  const year = currentYearAndQuarter?.year ?? -1;

  const contentClassName = `ppc-modal add-course-modal ${showAddCourse ? 'enter' : 'exit'}`;
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

  if (!activeCourse) {
    return (
      <>
        <div className={contentClassName}></div>
        {overlay}
      </>
    );
  }

  const { minUnits, maxUnits, department, courseNumber } = activeCourse;
  const numUnits = minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`;
  const unitText = `(${numUnits} unit${pluralize(maxUnits)})`;

  return (
    <>
      <div className={contentClassName}>
        <Modal.Header>
          <h2>
            {department} {courseNumber}
          </h2>
          <span className="unit-count">{unitText}</span>
          <CourseBookmarkButton course={activeCourse} />
          <div className="spacer"></div>
          <button onClick={closePopup} className="close-button">
            <CloseIcon />
          </button>
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
