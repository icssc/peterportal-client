import { FC } from 'react';
import './HitItem.scss';
import { useNavigate } from 'react-router-dom';
import CourseQuarterIndicator from '../../component/QuarterTooltip/CourseQuarterIndicator';
import Badge from 'react-bootstrap/Badge';

import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setCourse } from '../../store/slices/popupSlice';
import { CourseGQLData } from '../../types/types';
import { getCourseTags, useIsMobile, getCourseId } from '../../helpers/util';
import { useSavedCourses } from '../../hooks/savedCourses';

import { IconButton } from '@mui/material';
import BookmarkBorderIcon from '@mui/icons-material/BookmarkBorder';
import BookmarkIcon from '@mui/icons-material/Bookmark';

interface CourseHitItemProps {
  course: CourseGQLData;
  requiredCourses?: string[] | undefined; // Why is this not used, but is passed as a prop in SearchHitContainer?
}

const CourseHitItem: FC<CourseHitItemProps> = ({ course }) => {
  const dispatch = useAppDispatch();
  const navigate = useNavigate();
  const activeCourse = useAppSelector((state) => state.popup.course);
  const isMobile = useIsMobile();
  const { saveCourse, unsaveCourse, isCourseSaved } = useSavedCourses();
  const courseIsSaved = isCourseSaved(course);
  const pillData = getCourseTags(course); // data to be displayed in pills

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

  const onSaveCourse = (e: React.MouseEvent<HTMLButtonElement>) => {
    e.stopPropagation();
    if (!course || course.id === undefined || isCourseSaved(course)) return;
    saveCourse(course);
  };

  const onUnsaveCourse = (e: React.MouseEvent<HTMLButtonElement>) => {
    e.stopPropagation();
    unsaveCourse(course);
  };

  return (
    <div className="hit-item course-hit" tabIndex={0} role="button" onClick={onClickName} onKeyDown={onKeyDown}>
      <div className="course-hit-id">
        <div>
          <p className="hit-name">
            {getCourseId(course)} • {course.title}
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
          <IconButton onClick={(e) => (courseIsSaved ? onUnsaveCourse(e) : onSaveCourse(e))} size="small">
            {courseIsSaved ? <BookmarkIcon /> : <BookmarkBorderIcon />}
          </IconButton>
        </div>
      </div>
    </div>
  );
};

export default CourseHitItem;
