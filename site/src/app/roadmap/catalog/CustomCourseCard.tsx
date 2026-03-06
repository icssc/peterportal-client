import './CustomCourseCard.scss';
import { FC } from 'react';
import DragIndicatorIcon from '@mui/icons-material/DragIndicator';
import { IconButton } from '@mui/material';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import { useIsMobile } from '../../../helpers/util';
import { useAppDispatch } from '../../../store/hooks';
import { removeCustomCourse } from '../../../store/slices/customCourseSlice';

interface CustomCourseCardProps {
  courseName: string;
  units: number;
  description: string;
}

export const CustomCourseCard: FC<CustomCourseCardProps> = ({ courseName, units, description }) => {
  const dispatch = useAppDispatch();
  const isMobile = useIsMobile();
  const onDelete = () => {
    dispatch(removeCustomCourse(courseName));
  };

  return (
    <div className="course">
      {!isMobile && (
        <div className="course-drag-handle">
          <DragIndicatorIcon />
        </div>
      )}

      <div className="course-card-top">
        {courseName ? (
          <span className="name">{courseName}</span>
        ) : (
          <span className="name">
            <input className="name-input" placeholder="Course" />
          </span>
        )}

        <span className="units">
          {units ? `${units} units` : <input className="units-input" placeholder="Units" />}
        </span>

        <IconButton className="course-delete-btn" onClick={onDelete} aria-label="delete">
          <DeleteOutlineIcon className="course-delete-icon" />
        </IconButton>
      </div>
      <div className="course-description">
        {description ? `${description}` : <input className="input" placeholder="Description" />}
      </div>
    </div>
  );
};

export default CustomCourseCard;
