import './CustomCourseCard.scss';
import { FC, useCallback, useEffect, useState } from 'react';
import DragIndicatorIcon from '@mui/icons-material/DragIndicator';
import { IconButton } from '@mui/material';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import ModeEditIcon from '@mui/icons-material/ModeEdit';
import CheckIcon from '@mui/icons-material/Check';
import { useAppDispatch } from '../../../store/hooks';
import { removeCustomCourse } from '../../../store/slices/customCourseSlice';
import { CustomCourse } from '../../../types/types';
import { removeCustomCourseFromRoadmap } from '../../../store/slices/roadmapSlice';
import { useIsLoggedIn } from '../../../hooks/isLoggedIn';
import { pluralize } from '../../../helpers/util';
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

  const [editing, setEditing] = useState(false);
  const [saving, setSaving] = useState(false);

  const [newName, setNewName] = useState<string>(course.courseName);
  const [newUnits, setNewUnits] = useState<number>(course.units);
  const [newDescription, setNewDescription] = useState<string>(course.description);

  useEffect(() => {
    if (inRoadmap || editing) return;
    setNewName(course.courseName);
    setNewUnits(course.units);
    setNewDescription(course.description);
  }, [course.courseName, course.units, course.description, course.id, editing, inRoadmap]);

  const onDelete = useCallback(
    async (e?: React.MouseEvent) => {
      e?.stopPropagation();
      if (isLoggedIn) {
        await trpc.customCourses.deleteCustomCard.mutate(course.id);
      }
      dispatch(removeCustomCourse(course.id));
      dispatch(removeCustomCourseFromRoadmap(course.id));
    },
    [dispatch, course.id, isLoggedIn],
  );

  const handleStartEdit = (e: React.MouseEvent) => {
    e.stopPropagation();
    setNewName(course.courseName);
    setNewUnits(course.units);
    setNewDescription(course.description);
    setEditing(true);
  };

  const persistChanges = useCallback(async () => {
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
  }, [course, newDescription, newName, newUnits, handleUpdate, isLoggedIn]);

  const commitSave = async () => {
    if (saving) return;
    setSaving(true);
    try {
      await persistChanges();
      setEditing(false);
    } finally {
      setSaving(false);
    }
  };

  const handleSaveClick = (e: React.MouseEvent) => {
    e.stopPropagation();
    void commitSave();
  };

  const handleKeyDownEdit = (e: React.KeyboardEvent<HTMLInputElement>) => {
    if (e.key === 'Enter') {
      e.preventDefault();
      void commitSave();
    }
    if (e.key === 'Escape') {
      e.preventDefault();
      setNewName(course.courseName);
      setNewUnits(course.units);
      setNewDescription(course.description);
      setEditing(false);
    }
  };

  if (inRoadmap) {
    return (
      <div className="custom-card">
        <div className="course-drag-handle">
          <DragIndicatorIcon />
        </div>

        <div className="course-card-top">
          <span className="name">{course.courseName}</span>

          <span className="units">
            <>{course.units} units</>
          </span>

          <IconButton className="course-delete-btn" onClick={removeCourseAt} aria-label="delete">
            <DeleteOutlineIcon className="course-delete-icon" />
          </IconButton>
        </div>
        <div className="course-description">{course.description}</div>
      </div>
    );
  }

  return (
    <div className={'custom-card' + (!editing ? ' custom-card--view' : '')}>
      <div className="course-drag-handle">
        <DragIndicatorIcon />
      </div>

      {editing ? (
        <>
          <div className="course-card-top">
            <span className="name">
              <input
                className="name-input"
                value={newName ?? ''}
                placeholder="Course"
                onChange={(e) => setNewName(e.target.value)}
                onKeyDown={handleKeyDownEdit}
                onClick={(e) => e.stopPropagation()}
              />
            </span>

            <span className="units">
              <input
                className="units-input"
                type="number"
                min="0"
                value={Number.isFinite(newUnits) ? newUnits : ''}
                placeholder="Units"
                onChange={(e) => {
                  const v = e.target.valueAsNumber;
                  setNewUnits(Number.isFinite(v) ? v : NaN);
                }}
                onKeyDown={handleKeyDownEdit}
                onClick={(e) => e.stopPropagation()}
              />
            </span>

            <IconButton
              className="course-save-btn"
              onClick={handleSaveClick}
              aria-label="Save changes"
              disabled={saving}
            >
              <CheckIcon />
            </IconButton>

            <IconButton className="course-delete-btn" onClick={(e) => void onDelete(e)} aria-label="delete">
              <DeleteOutlineIcon className="course-delete-icon" />
            </IconButton>
          </div>
          <div className="course-description">
            <input
              className="description-input"
              value={newDescription ?? ''}
              placeholder="Description"
              onChange={(e) => setNewDescription(e.target.value)}
              onKeyDown={handleKeyDownEdit}
              onClick={(e) => e.stopPropagation()}
            />
          </div>
        </>
      ) : (
        <>
          <div className="course-card-top">
            <span className="name name--display">{course.courseName.trim() ? course.courseName : 'Course'}</span>
            <div className="course-card-meta">
              <span className="units-display">
                {course.units} {pluralize(course.units, 'units', 'unit')}
              </span>
              <IconButton className="course-edit-btn" onClick={handleStartEdit} aria-label="Edit custom card">
                <ModeEditIcon />
              </IconButton>
              <IconButton className="course-delete-btn" onClick={(e) => void onDelete(e)} aria-label="delete">
                <DeleteOutlineIcon className="course-delete-icon" />
              </IconButton>
            </div>
          </div>
          <div className="course-description course-description--view">
            {course.description.trim() ? (
              course.description
            ) : (
              <span className="description-placeholder">Description</span>
            )}
          </div>
        </>
      )}
    </div>
  );
};

export default CustomCourseCard;
