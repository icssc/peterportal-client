import './CustomCourseCard.scss';
import { FC, useCallback, useState } from 'react';
import DragIndicatorIcon from '@mui/icons-material/DragIndicator';
import { IconButton } from '@mui/material';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import { useAppDispatch } from '../../../store/hooks';
import { removeCustomCourse } from '../../../store/slices/customCourseSlice';
import { CustomCourse } from '../../../types/types';
import { removeCustomCourseFromRoadmap } from '../../../store/slices/roadmapSlice';

interface CustomCourseCardProps {
  course: CustomCourse;
  handleUpdate: (customCourse: CustomCourse) => void;
  inRoadmap: boolean;
  removeCourseAt?: () => void;
}

export const CustomCourseCard: FC<CustomCourseCardProps> = ({ course, handleUpdate, inRoadmap, removeCourseAt }) => {
  const dispatch = useAppDispatch();
  const [newName, setNewName] = useState<string>(course.courseName);
  const [newUnits, setNewUnits] = useState<number>(course.units);
  const [newDescription, setNewDescription] = useState<string>(course.description);

  const onDelete = useCallback(() => {
    dispatch(removeCustomCourse(course.id));
    dispatch(removeCustomCourseFromRoadmap(course.id));
  }, [dispatch, course.id]);

  const handleKeyDown = (event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key === 'Enter') (event.target as HTMLInputElement).blur();
  };

  const onBlur = () => {
    if (Number.isNaN(newUnits)) {
      setNewUnits(course.units);
      return;
    }

    handleUpdate({ ...course, courseName: newName, units: newUnits, description: newDescription });
  };

  return (
    <div className="custom-card">
      <div className="course-drag-handle">
        <DragIndicatorIcon />
      </div>

      <div className="course-card-top">
        <span className="name">
          {!inRoadmap ? (
            <input
              className="name-input"
              value={newName ?? ''}
              placeholder="Course"
              onChange={(e) => setNewName(e.target.value)}
              onKeyDown={handleKeyDown}
              onBlur={onBlur}
            />
          ) : (
            <>{course.courseName}</>
          )}
        </span>

        <span className="units">
          {!inRoadmap ? (
            <input
              className="units-input"
              type="number"
              min="0"
              value={newUnits}
              placeholder="Units"
              onChange={(e) => setNewUnits(e.target.valueAsNumber)}
              onKeyDown={handleKeyDown}
              onBlur={onBlur}
            />
          ) : (
            <>{course.units} units</>
          )}
        </span>

        <IconButton className="course-delete-btn" onClick={!inRoadmap ? onDelete : removeCourseAt} aria-label="delete">
          <DeleteOutlineIcon className="course-delete-icon" />
        </IconButton>
      </div>
      <div className="course-description">
        {!inRoadmap ? (
          <input
            className="description-input"
            value={newDescription ?? ''}
            placeholder="Description"
            onChange={(e) => setNewDescription(e.target.value)}
            onKeyDown={handleKeyDown}
            onBlur={onBlur}
          />
        ) : (
          <>{course.description}</>
        )}
      </div>
    </div>
  );
};

export default CustomCourseCard;
