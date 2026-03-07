import './Library.scss';
import { useState, useCallback } from 'react';
import { ExpandMore } from '../../../component/ExpandMore/ExpandMore';
import SavedCourseList from './SavedCourses';
import { Collapse } from '@mui/material';
import AddIcon from '@mui/icons-material/Add';
import CustomCourseCard from './CustomCourseCard';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { addCustomCourse, updateCustomCourse, reorderCustomCourses } from '../../../store/slices/customCourseSlice';
import { ReactSortable } from 'react-sortablejs';
import { deepCopy } from '../../../helpers/util';

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
  const customCoursesCopy = deepCopy(userCustomCourses);

  const addCard = useCallback(() => {
    /** @todo replace id with actual id */
    const newId = Date.now();
    dispatch(addCustomCourse({ id: newId, courseName: '', units: 0, description: '' }));
  }, [dispatch]);

  const handleUpdate = (id: number, courseName: string, units: number, description: string) => {
    dispatch(updateCustomCourse({ id, courseName, units, description }));
  };

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
          <ReactSortable
            list={customCoursesCopy}
            setList={() => {}}
            handle=".course-drag-handle"
            onEnd={(evt) => {
              if (evt.oldIndex == null || evt.newIndex == null) return;
              if (evt.oldIndex === evt.newIndex) return;

              dispatch(
                reorderCustomCourses({
                  oldIndex: evt.oldIndex,
                  newIndex: evt.newIndex,
                }),
              );
            }}
          >
            {userCustomCourses.map((course) => (
              <CustomCourseCard
                key={course.id}
                id={course.id}
                courseName={course.courseName}
                units={course.units}
                description={course.description}
                handleUpdate={handleUpdate}
              />
            ))}
          </ReactSortable>

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
