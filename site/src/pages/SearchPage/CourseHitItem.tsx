import { FC } from 'react';
import { useNavigate } from 'react-router-dom';
import Badge from 'react-bootstrap/Badge';
import './HitItem.scss';

import CourseQuarterIndicator from '../../component/QuarterTooltip/CourseQuarterIndicator';
import { CourseGQLData } from '../../types/types';
import { setCourse } from '../../store/slices/popupSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { useCoursebag } from '../../hooks/coursebag';
import { getCourseTags, useIsMobile } from '../../helpers/util';

import { IconButton } from '@mui/material';
import BookmarkBorderIcon from '@mui/icons-material/BookmarkBorder';
import BookmarkIcon from '@mui/icons-material/Bookmark';

interface CourseHitItemProps {
  course: CourseGQLData;
  requiredCourses?: string[]; // Why is this not used, but is passed as a prop in SearchHitContainer?
}

const CourseHitItem: FC<CourseHitItemProps> = ({ course }) => {
  const dispatch = useAppDispatch();
  const navigate = useNavigate();
  const activeCourse = useAppSelector((state) => state.popup.course);
  const isMobile = useIsMobile();
  const { coursebag, addCourseToBag, removeCourseFromBag } = useCoursebag();
  const isInBag = coursebag.some((c) => c.id === course.id);
  const pillData = getCourseTags(course);

  const onClickName = () => {
    // set the popup course
    dispatch(setCourse(course));
    // if click on a course that is already in popup or if on mobile
    if ((activeCourse && course.id == activeCourse.id) || isMobile) {
      navigate(`/course/${course.id}`);
    }
  };

  const onKeyDown = (e: React.KeyboardEvent<HTMLDivElement>) => {
    if (e.key === 'Enter') {
      onClickName();
    }
  };

  const onAddToBag = (e: React.MouseEvent<HTMLButtonElement>) => {
    e.stopPropagation();
    if (!course || course.id === undefined || coursebag.some((c) => c.id === course.id)) return;
    addCourseToBag(course);
  };

  const removeFromBag = (e: React.MouseEvent<HTMLButtonElement>) => {
    e.stopPropagation();
    removeCourseFromBag(course);
  };

  return (
    <div className="hit-item course-hit" tabIndex={0} role="button" onClick={onClickName} onKeyDown={onKeyDown}>
      <div className="course-hit-id">
        <div>
          <p className="hit-name">
            {course.department} {course.courseNumber} • {course.title}
          </p>
          <CourseQuarterIndicator terms={course.terms} size="sm" />
        </div>
        <p className="hit-subtitle">{course.school}</p>
      </div>

      <div>
        <p className="description">{course.description}</p>
        <div className="hit-lower">
          <div>
            {pillData.map((pill, i) => (
              <Badge key={`course-hit-item-pill-${i}`} pill className="badge" variant="info">
                {pill}
              </Badge>
            ))}
          </div>
          <IconButton onClick={(e) => (isInBag ? removeFromBag(e) : onAddToBag(e))} size="small">
            {isInBag ? <BookmarkIcon /> : <BookmarkBorderIcon />}
          </IconButton>
        </div>
      </div>
    </div>
  );
};

export default CourseHitItem;
