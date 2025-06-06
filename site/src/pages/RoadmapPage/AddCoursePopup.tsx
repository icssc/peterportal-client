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
import { getUnitText } from '../../helpers/util';

import CloseIcon from '@mui/icons-material/Close';

interface AddCoursePopupContentProps {
  closePopup: () => void;
}

const AddCoursePopupContent: FC<AddCoursePopupContentProps> = ({ closePopup }) => {
  const activeCourse = useAppSelector((state) => state.roadmap.activeCourse);
  const currentYearAndQuarter = useAppSelector((state) => state.roadmap.currentYearAndQuarter);
  const activeMissingPrerequisites = useAppSelector((state) => state.roadmap.activeMissingPrerequisites);
  const showAddCourse = useAppSelector((state) => state.roadmap.showAddCourse);

  const dispatch = useAppDispatch();
  const term = useNamedAcademicTerm();

  if (!activeCourse) return null;

  const addToRoadmap = () => {
    const year = currentYearAndQuarter?.year ?? -1;
    const quarter = currentYearAndQuarter?.quarter ?? -1;

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

  const { department, courseNumber } = activeCourse;
  const unitText = getUnitText(activeCourse);

  return (
    <div className={`ppc-modal add-course-modal ${showAddCourse ? 'enter' : 'exit'}`}>
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
  );
};

const AddCoursePopup: FC = () => {
  const dispatch = useAppDispatch();
  const closePopup = () => dispatch(setShowAddCourse(false));

  return (
    <>
      <AddCoursePopupContent closePopup={closePopup} />
      <UIOverlay onClick={closePopup} zIndex={499} />
    </>
  );
};

export default AddCoursePopup;
