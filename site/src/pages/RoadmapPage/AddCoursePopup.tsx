import { FC } from 'react';
import Modal from 'react-bootstrap/Modal';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { moveCourse, setShowAddCourse, setShowSearch } from '../../store/slices/roadmapSlice';
import './AddCoursePopup.scss';
import UIOverlay from '../../component/UIOverlay/UIOverlay';
import { useNamedAcademicTerm } from '../../hooks/namedAcademicTerm';
import { CourseHeader, AllCourseInfo } from '../../component/CourseInfo/CourseInfo';

const AddCoursePopupContent = () => {
  const activeCourse = useAppSelector((state) => state.roadmap.activeCourse);
  const currentYearAndQuarter = useAppSelector((state) => state.roadmap.currentYearAndQuarter);
  const activeMissingPrerequisites = useAppSelector((state) => state.roadmap.activeMissingPrerequisites);
  const showAddCourse = useAppSelector((state) => state.roadmap.showAddCourse);

  const dispatch = useAppDispatch();
  const term = useNamedAcademicTerm();

  if (!activeCourse) return null;

  const addToRoadmap = () => {
    // add course to roadmap
    dispatch(
      moveCourse({
        from: { yearIndex: -1, quarterIndex: -1, courseIndex: -1 },
        to: {
          yearIndex: currentYearAndQuarter?.year ?? -1,
          quarterIndex: currentYearAndQuarter?.quarter ?? -1,
          courseIndex: 0,
        },
      }),
    );
    // hide the search bar to view the roadmap
    dispatch(setShowSearch({ show: false }));
    dispatch(setShowAddCourse(false));
  };

  return (
    <div className={`ppc-modal add-course-modal ${showAddCourse ? 'enter' : 'exit'}`}>
      <Modal.Header>
        <CourseHeader course={activeCourse} />
      </Modal.Header>
      <Modal.Body>
        <AllCourseInfo course={activeCourse} missingPrerequisites={activeMissingPrerequisites} />
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
      <AddCoursePopupContent />
      <UIOverlay onClick={closePopup} zIndex={499} />
    </>
  );
};

export default AddCoursePopup;
