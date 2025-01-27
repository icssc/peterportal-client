import { FC } from 'react';
import Modal from 'react-bootstrap/Modal';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { moveCourse, setShowAddCourse, setShowSearch } from '../../store/slices/roadmapSlice';
import './AddCoursePopup.scss';
import { useCoursebag } from '../../hooks/coursebag';
import { Bookmark, BookmarkFill, X } from 'react-bootstrap-icons';
import UIOverlay from '../../component/UIOverlay/UIOverlay';
import { useNamedAcademicTerm } from '../../hooks/namedAcademicTerm';
import CourseQuarterIndicator from '../../component/QuarterTooltip/CourseQuarterIndicator';

interface AddCoursePopupProps {}

const AddCoursePopup: FC<AddCoursePopupProps> = () => {
  const currentYearAndQuarter = useAppSelector((state) => state.roadmap.currentYearAndQuarter);
  const { coursebag, addCourseToBag, removeCourseFromBag } = useCoursebag();
  const showAddCourse = useAppSelector((state) => state.roadmap.showAddCourse);
  const activeCourse = useAppSelector((state) => state.roadmap.activeCourse);
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
      <div className={contentClassName}>
        <Modal.Header>
          <h2>
            {department} {courseNumber}
          </h2>
          <span className="unit-count">
            ({minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`} unit{maxUnits === 1 ? '' : 's'})
          </span>
          <button onClick={toggleSaved}>{inCourseBag ? <BookmarkFill /> : <Bookmark />}</button>
          <div className="spacer"></div>
          <button onClick={closePopup} className="close-button">
            <X width={32} height={32} />
          </button>
        </Modal.Header>
        <Modal.Body>
          <p>
            <b>{title}:</b> {description}
          </p>
          <p className="quarter-offerings-section">
            <b>Previous Offerings:</b>
            <CourseQuarterIndicator terms={activeCourse.terms} size="sm" />
          </p>
          {/** @todo Add UnmetPrerequisiteText for prerequisites that don't exist in the planner */}
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
