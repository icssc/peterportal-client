import './CustomCourseCard.scss';
import { FC, useCallback, useState } from 'react';
import DragIndicatorIcon from '@mui/icons-material/DragIndicator';
import { IconButton, TextField } from '@mui/material';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import { useAppDispatch } from '../../../store/hooks';
import { removeCustomCourse } from '../../../store/slices/customCourseSlice';
import { CustomCourse } from '../../../types/types';
import { removeCustomCourseFromRoadmap } from '../../../store/slices/roadmapSlice';
import { useIsLoggedIn } from '../../../hooks/isLoggedIn';
import trpc from '../../../trpc';

interface CustomCourseCardProps {
  course: CustomCourse;
  handleUpdate: (customCourse: CustomCourse) => void;
  inRoadmap: boolean;
  removeCourseAt?: () => void;
}

export const CustomCourseCard: FC<CustomCourseCardProps> = ({ course, handleUpdate, inRoadmap, removeCourseAt }) => {
  const dispatch = useAppDispatch();
  const isLoggedIn = useIsLoggedIn();

  const [newName, setNewName] = useState<string>(course.courseName);
  const [newUnits, setNewUnits] = useState<number>(course.units);
  const [newDescription, setNewDescription] = useState<string>(course.description);

  const onDelete = useCallback(async () => {
    if (isLoggedIn) {
      await trpc.customCourses.deleteCustomCard.mutate(course.id);
    }
    dispatch(removeCustomCourse(course.id));
    dispatch(removeCustomCourseFromRoadmap(course.id));
  }, [dispatch, course.id, isLoggedIn]);

  const handleKeyDown = (event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key === 'Enter') (event.target as HTMLInputElement).blur();
  };

  const onBlur = async () => {
    const rawUnits = newUnits;
    const units = Number.isFinite(rawUnits) && rawUnits >= 0 ? rawUnits : 0;
    const updated: CustomCourse = {
      ...course,
      courseName: newName ?? '',
      units,
      description: newDescription ?? '',
    };

    if (!isLoggedIn) {
      handleUpdate(updated);
      return;
    }

    await trpc.customCourses.editCustomCard.mutate({
      id: updated.id,
      name: updated.courseName,
      description: updated.description,
      units: updated.units,
    });

    handleUpdate(updated);
  };

  return (
    <div className="custom-card">
      <div className="course-drag-handle">
        <DragIndicatorIcon />
      </div>

      <div className="course-card-top">
        <span className="name">
          {!inRoadmap ? (
            <TextField
              className="name-input"
              value={newName ?? ''}
              placeholder="Course"
              onChange={(e) => setNewName(e.target.value)}
              onKeyDown={handleKeyDown}
              onBlur={onBlur}
            />
          ) : (
            course.courseName
          )}
        </span>

        <span className="units">
          {!inRoadmap ? (
            <TextField
              className="units-input"
              type="number"
              value={Number.isFinite(newUnits) ? newUnits : ''}
              placeholder="Units"
              onChange={(e) => {
                const v = Number(e.target.value);
                setNewUnits(Number.isFinite(v) ? v : NaN);
              }}
              onKeyDown={handleKeyDown}
              onBlur={onBlur}
              slotProps={{
                htmlInput: {
                  min: 0,
                },
              }}
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
          <TextField
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
