import './CustomCourseCard.scss';
import { FC, useCallback, useState } from 'react';
import DragIndicatorIcon from '@mui/icons-material/DragIndicator';
import { IconButton } from '@mui/material';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import { useIsMobile } from '../../../helpers/util';
import { useAppDispatch } from '../../../store/hooks';
import { removeCustomCourse } from '../../../store/slices/customCourseSlice';

interface CustomCourseCardProps {
  cardId: number;
  courseName: string;
  units: number;
  description: string;
  handleUpdate: (cardId: number, courseName: string, units: number, description: string) => void;
}

export const CustomCourseCard: FC<CustomCourseCardProps> = ({
  cardId,
  courseName,
  units,
  description,
  handleUpdate,
}) => {
  const dispatch = useAppDispatch();
  const isMobile = useIsMobile();
  const [newName, setNewName] = useState<string>(courseName);
  const [newUnits, setNewUnits] = useState<number>(units);
  const [newDescription, setNewDescription] = useState<string>(description);

  const onDelete = useCallback(() => {
    dispatch(removeCustomCourse(cardId));
  }, [dispatch, cardId]);

  const handleKeyDown = (event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key === 'Enter') (event.target as HTMLInputElement).blur();
  };

  const onBlur = () => {
    handleUpdate(cardId, newName, newUnits, newDescription);
  };

  return (
    <div className="course">
      {!isMobile && (
        <div className="course-drag-handle">
          <DragIndicatorIcon />
        </div>
      )}

      <div className="course-card-top">
        <span className="name">
          <input
            className="name-input"
            value={newName ?? ''}
            placeholder="Course"
            onChange={(e) => setNewName(e.target.value)}
            onKeyDown={handleKeyDown}
            onBlur={onBlur}
          />
        </span>

        <span className="units">
          <input
            className="units-input"
            type="number"
            value={Number.isNaN(newUnits) ? '' : newUnits}
            placeholder="Units"
            onChange={(e) => setNewUnits(e.target.valueAsNumber)}
            onKeyDown={handleKeyDown}
            onBlur={onBlur}
          />
        </span>

        <IconButton className="course-delete-btn" onClick={onDelete} aria-label="delete">
          <DeleteOutlineIcon className="course-delete-icon" />
        </IconButton>
      </div>
      <div className="course-description">
        <input
          className="description-input"
          value={newDescription ?? ''}
          placeholder="Description"
          onChange={(e) => setNewDescription(e.target.value)}
          onKeyDown={handleKeyDown}
          onBlur={onBlur}
        />
      </div>
    </div>
  );
};

export default CustomCourseCard;
