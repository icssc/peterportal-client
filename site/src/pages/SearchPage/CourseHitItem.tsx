import { FC } from 'react';
import './HitItem.scss';
import { useNavigate } from 'react-router-dom';
import CourseQuarterIndicator from '../../component/QuarterTooltip/CourseQuarterIndicator';
import Badge from 'react-bootstrap/Badge';

import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setCourse } from '../../store/slices/popupSlice';
import { CourseGQLData } from '../../types/types';
import { getCourseTags, useIsMobile } from '../../helpers/util';
import { BagFill, BagPlus } from 'react-bootstrap-icons';
import { addCourseToBag, removeCourseFromBag } from '../../store/slices/roadmapSlice';
interface CourseHitItemProps extends CourseGQLData {}

const CourseHitItem: FC<CourseHitItemProps> = (props) => {
  const dispatch = useAppDispatch();
  const navigate = useNavigate();
  const activeCourse = useAppSelector((state) => state.popup.course);
  const isMobile = useIsMobile();
  const coursebag = useAppSelector((state) => state.roadmap.coursebag);
  const isInBag = coursebag.some((course) => course.id === props.id);

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

  const onKeyDown = (e: React.KeyboardEvent<HTMLDivElement>) => {
    if (e.key === 'Enter') {
      onClickName();
    }
  };

  const onAddToBag = (e: React.MouseEvent<SVGElement, MouseEvent>) => {
    e.stopPropagation();
    if (!props) return;
    if (props.id === undefined) return;
    if (coursebag.some((course) => course.id === props.id)) return;
    dispatch(addCourseToBag(props));
  };

  const removeFromBag = (e: React.MouseEvent<SVGElement, MouseEvent>) => {
    e.stopPropagation();
    dispatch(removeCourseFromBag(props));
  };

  return (
    <div className="hit-item" tabIndex={0} role="button" onClick={onClickName} onKeyDown={onKeyDown}>
      <div className="course-hit-id">
        <div>
          <h3>
            {props.department} {props.courseNumber} {props.title}
          </h3>
        </div>
        <CourseQuarterIndicator terms={props.terms} size="sm" />
      </div>

      <div style={{ zIndex: 1000 }}>
        <h4 className="hit-subtitle">{props.school}</h4>
        <p>{props.description}</p>
        <div className="hit-lower">
          <div className="hit-badges">
            {pillData.map((pill, i) => (
              <Badge key={`course-hit-item-pill-${i}`} pill className="p-2 mr-3" variant="info">
                {pill}
              </Badge>
            ))}
          </div>
          <div>
            {onAddToBag && !isInBag && <BagPlus onClick={(e) => onAddToBag(e)} size={24}></BagPlus>}
            {isInBag && <BagFill size={24} onClick={(e) => removeFromBag(e)}></BagFill>}
          </div>
        </div>
      </div>
    </div>
  );
};

export default CourseHitItem;
