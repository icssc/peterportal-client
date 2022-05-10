import React, { FC, useState, useEffect } from 'react';
import './CourseHitItem.scss';
import { useHistory } from 'react-router-dom';
import CourseQuarterIndicator from './CourseQuarterIndicator';
import Badge from 'react-bootstrap/Badge'
import { isMobile } from 'react-device-detect';

import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setCourse } from '../../store/slices/popupSlice';
import { CourseData } from '../../types/types';
import { getCourseTags } from '../../helpers/util';

interface CourseHitItemProps extends CourseData {
}

const CourseHitItem: FC<CourseHitItemProps> = (props) => {
  const dispatch = useAppDispatch();
  const history = useHistory();
  const activeCourse = useAppSelector(state => state.popup.course);

  // data to be displayed in pills
  let pillData = getCourseTags(props);

  const onClickName = () => {
    // if click on a course that is already in popup
    // or if on mobile
    if (activeCourse && props.id == activeCourse.id || isMobile) {
      history.push(`/course/${props.id}`)
    }
    // click on new or different course than popup
    else {
      dispatch(setCourse(props))
    }
  }

  return (
    <div className='course-hit-item' onClick={onClickName}>
      <div className='course-hit-id'>
        <div>
          <h3>
            <span>{props.department}</span>
            &nbsp;
            <span>{props.number}</span>
            &nbsp;
            <span>{props.title}</span>
          </h3>
        </div>
        <CourseQuarterIndicator terms={props.terms} />
      </div>

      <div>
        <h4 className='hit-subtitle'>
          {props.school}
        </h4>

        <p>{props.description}</p>

        <div className='hit-badges'>
          {
            pillData.map((pill, i) => <Badge key={`course-hit-item-pill-${i}`} pill className='p-2 mr-3' variant='info'>
              {pill}
            </Badge>)
          }
        </div>
      </div>
    </div>
  )
};

export default CourseHitItem;