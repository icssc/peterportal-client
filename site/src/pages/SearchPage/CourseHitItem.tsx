import { FC } from 'react';
import './HitItem.scss';
import { useNavigate } from 'react-router-dom';
import CourseQuarterIndicator from './CourseQuarterIndicator';
import Badge from 'react-bootstrap/Badge';
import { isMobile } from 'react-device-detect';

import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setCourse } from '../../store/slices/popupSlice';
import { CourseGQLData } from '../../types/types';
import { getCourseTags } from '../../helpers/util';

interface CourseHitItemProps extends CourseGQLData {}

const CourseHitItem: FC<CourseHitItemProps> = (props) => {
  const dispatch = useAppDispatch();
  const navigate = useNavigate();
  const activeCourse = useAppSelector((state) => state.popup.course);

  // data to be displayed in pills
  const pillData = getCourseTags(props);

  const onClickName = () => {
    // set the popup course
    dispatch(setCourse(props));

    // if click on a course that is already in popup
    // or if on mobile
    if ((activeCourse && props.id == activeCourse.id) || isMobile) {
      navigate(`/course/${props.id}`);
    }
  };

  return (
    <div className="hit-item" onClick={onClickName}>
      <div className="course-hit-id">
        <div>
          <h3>
            {props.department} {props.courseNumber} {props.title}
          </h3>
        </div>
        <CourseQuarterIndicator terms={props.terms} />
      </div>

      <div>
        <h4 className="hit-subtitle">{props.school}</h4>

        <p>{props.description}</p>

        <div className="hit-badges">
          {pillData.map((pill, i) => (
            <Badge key={`course-hit-item-pill-${i}`} pill className="p-2 mr-3" variant="info">
              {pill}
            </Badge>
          ))}
        </div>
      </div>
    </div>
  );
};

export default CourseHitItem;
