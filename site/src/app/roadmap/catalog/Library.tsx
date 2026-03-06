import './Library.scss';
import { useEffect, useState, useCallback } from 'react';
import { ExpandMore } from '../../../component/ExpandMore/ExpandMore';
import SavedCourseList from './SavedCourses';
import { Collapse } from '@mui/material';
import AddIcon from '@mui/icons-material/Add';
import CustomCourseCard from './CustomCourseCard';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { addCustomCourse } from '../../../store/slices/customCourseSlice';

const SavedCourses = () => {
  const [open, setOpen] = useState(true);
  const toggleExpand = () => setOpen(!open);
  return (
    <div className="saved-courses">
      <div
        className="header-tab"
        role="button"
        tabIndex={0}
        onClick={toggleExpand}
        onKeyDown={(e) => {
          if (e.key === 'Enter' || e.key === ' ') toggleExpand();
        }}
      >
        <h4>Saved</h4>
        <ExpandMore expanded={open} onClick={toggleExpand} />
      </div>
      <Collapse in={open} unmountOnExit>
        <div className="section-content">
          <SavedCourseList />
        </div>
      </Collapse>
    </div>
  );
};

const CustomCourses = () => {
  const [open, setOpen] = useState(true);
  const toggleExpand = () => setOpen(!open);
  const dispatch = useAppDispatch();
  const userCustomCourses = useAppSelector((state) => state.customCourses.userCustomCourses);

  useEffect(() => {
    dispatch(addCustomCourse({ courseName: 'Custom Card Test', units: 4, description: 'Description' }));
  }, [dispatch]);

  const addCard = useCallback(() => {
    dispatch(addCustomCourse({ courseName: '', units: 0, description: '' }));
  }, [dispatch]);

  return (
    <div className="custom-courses">
      <div
        className="header-tab"
        role="button"
        tabIndex={0}
        onClick={toggleExpand}
        onKeyDown={(e) => {
          if (e.key === 'Enter' || e.key === ' ') toggleExpand();
        }}
      >
        <h4>Custom Cards</h4>
        <ExpandMore expanded={open} onClick={toggleExpand} />
      </div>
      <Collapse in={open} unmountOnExit>
        <div className="section-content">
          {userCustomCourses.map((course) => (
            <CustomCourseCard
              key={course.courseName}
              courseName={course.courseName}
              units={course.units}
              description={course.description}
            />
          ))}
          <CustomCourseCard courseName="" units={0} description="" />
          <button className="add-card-button" type="button" onClick={addCard}>
            <AddIcon />
          </button>
        </div>
      </Collapse>
    </div>
  );
};

export const Library = () => {
  return (
    <div className="library">
      <SavedCourses />
      <CustomCourses />
    </div>
  );
};

export default Library;
