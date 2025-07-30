import { FC } from 'react';
import './HitItem.scss';
import { useNavigate } from 'react-router-dom';
import RecentOfferingsTooltip from '../../component/RecentOfferingsTooltip/RecentOfferingsTooltip';
import Badge from 'react-bootstrap/Badge';

import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setCourse } from '../../store/slices/popupSlice';
import { CourseGQLData } from '../../types/types';
import { getCourseTags, useIsMobile } from '../../helpers/util';
import { useSavedCourses } from '../../hooks/savedCourses';

import { IconButton } from '@mui/material';
import BookmarkBorderIcon from '@mui/icons-material/BookmarkBorder';
import BookmarkIcon from '@mui/icons-material/Bookmark';

interface CourseHitItemProps extends CourseGQLData {}

const CourseHitItem: FC<CourseHitItemProps> = (props) => {
  const dispatch = useAppDispatch();
  const navigate = useNavigate();
  const activeCourse = useAppSelector((state) => state.popup.course);
  const isMobile = useIsMobile();
  const { saveCourse, unsaveCourse, isCourseSaved } = useSavedCourses();
  const courseIsSaved = isCourseSaved(props);
  const pillData = getCourseTags(props); // data to be displayed in pills

  const onClickName = () => {
    // set the popup course
    dispatch(setCourse(props));

    // if click on a course that is already in popup
    // or if on mobile
    if ((activeCourse && props.id == activeCourse.id) || isMobile) {
      navigate(`/course/${props.id}`);
    }
  };

  const onKeyDown = (e: React.KeyboardEvent<HTMLDivElement>) => {
    if (e.key === 'Enter') {
      onClickName();
    }
  };

  const toggleSaveCourse = (e: React.MouseEvent<HTMLButtonElement>) => {
    e.stopPropagation();
    if (courseIsSaved) {
      unsaveCourse(props);
    } else if (props && props.id) {
      saveCourse(props);
    }
  };

  return (
    <div className="hit-item course-hit" tabIndex={0} role="button" onClick={onClickName} onKeyDown={onKeyDown}>
      <div className="course-hit-id">
        <div>
          <p className="hit-name">
            {props.department} {props.courseNumber} • {props.title}
          </p>
          <div className="hit-tooltip">
            <RecentOfferingsTooltip terms={props.terms} />
          </div>
        </div>
        <p className="hit-subtitle">{props.school}</p>
      </div>

      <div>
        <p className="description">{props.description}</p>
        <div className="hit-lower">
          <div className="hit-badges">
            {pillData.map((pill, i) => (
              <Badge key={`course-hit-item-pill-${i}`} pill className="badge" variant="info">
                {pill}
              </Badge>
            ))}
          </div>
          <IconButton onClick={toggleSaveCourse} size="small">
            {courseIsSaved ? <BookmarkIcon /> : <BookmarkBorderIcon />}
          </IconButton>
        </div>
      </div>
    </div>
  );
};

export default CourseHitItem;
