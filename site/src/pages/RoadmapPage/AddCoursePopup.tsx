import { FC } from 'react';
import Modal from 'react-bootstrap/Modal';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { moveCourse, setShowAddCourse, setShowSearch } from '../../store/slices/roadmapSlice';
import './AddCoursePopup.scss';
import { useCoursebag } from '../../hooks/coursebag';
import { Bookmark, BookmarkFill } from 'react-bootstrap-icons';
import UIOverlay from '../../component/UIOverlay/UIOverlay';
import { CSSTransition } from 'react-transition-group';
import { useNamedAcademicTerm } from '../../hooks/namedAcademicTerm';

interface AddCoursePopupProps {}

const AddCoursePopup: FC<AddCoursePopupProps> = () => {
  const currentYearAndQuarter = useAppSelector((state) => state.roadmap.currentYearAndQuarter);
  const { coursebag, addCourseToBag, removeCourseFromBag } = useCoursebag();
  const showAddCourse = useAppSelector((state) => state.roadmap.showAddCourse);
  const term = useNamedAcademicTerm();

  const dispatch = useAppDispatch();

  const quarter = currentYearAndQuarter?.quarter ?? -1;
  const year = currentYearAndQuarter?.year ?? -1;

  const activeCourse = useAppSelector((state) => state.roadmap.activeCourse);

  const closePopup = () => dispatch(setShowAddCourse(false));

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

  if (!activeCourse) return <></>;

  const inCourseBag = coursebag.some((course) => course.id === activeCourse.id);

  const toggleSaved = () => {
    if (inCourseBag) {
      removeCourseFromBag(activeCourse);
    } else {
      addCourseToBag(activeCourse);
    }
  };

  const { minUnits, maxUnits, department, courseNumber, title, description } = activeCourse;

  return (
    <>
      <CSSTransition in={showAddCourse} timeout={500} unmountOnExit>
        {/* enter-active conditional class is required for CSSTransition to apply classes properly */}
        <div className={'ppc-modal add-course-modal ' + (showAddCourse ? 'enter-active' : '')}>
          <Modal.Header>
            <h2>
              {department} {courseNumber}
            </h2>
            ({minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`})
            <button onClick={toggleSaved}>{inCourseBag ? <BookmarkFill /> : <Bookmark />}</button>
          </Modal.Header>
          <Modal.Body>
            <p>
              <b>{title}:</b> {description}
            </p>
          </Modal.Body>
          <button className="fixed" onClick={addToRoadmap}>
            Add to {term.quarter} {term.year}
          </button>
        </div>
      </CSSTransition>
      <UIOverlay onClick={closePopup} zIndex={499} />
    </>
  );
};

export default AddCoursePopup;
