import React, { useState, useEffect } from 'react';
import { useHistory } from 'react-router';
import { get } from 'lodash';
import CourseQuarterIndicator from './CourseQuarterIndicator';
import { RenderComponentType, HitItemProps } from 'searchkit';
import Badge from 'react-bootstrap/Badge'

import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setCourse } from '../../store/slices/popupSlice';
import { CourseData } from '../../types/types';

interface CourseHitItemProps extends HitItemProps {
  result: {
    _id: string;
    _index: string;
    _score: number;
    _source: CourseData;
    _type: string;
  }
}

const CourseHitItem: RenderComponentType<CourseHitItemProps> = (props: CourseHitItemProps) => {
  const dispatch = useAppDispatch();
  const history = useHistory();
  const activeCourse = useAppSelector(state => state.popup.course);

  useEffect(() => {
    window.scrollTo(0, 0);
  }, [])

  console.log(props.result._source)
  // data to be displayed in pills
  let pillData = [];
  // course level
  let courseLevel = props.result._source.course_level;
  if (courseLevel) {
    pillData.push(`${courseLevel.substring(0, courseLevel.indexOf('('))}`);
  }
  // ge
  props.result._source.ge_list.forEach(ge => {
    pillData.push(`${ge.substring(0, ge.indexOf(':'))}`);
  })
  // units
  let units = props.result._source.units[0]
  pillData.push(`${units} unit${units != 1 ? 's' : ''}`);

  return (
    <div className='hit-item'>
      <div style={{ display: 'flex' }}>
        <div>
          <a href='#' onClick={() => {
            // if click on a course that is already in popup
            if (activeCourse && props.result._source.id == activeCourse.id) {
              history.push(`/course/${activeCourse.id}`)
            }
            // click on new or different course than popup
            else {
              dispatch(setCourse(props.result._source))
            }
          }}>
            <h3>
              <span
                className={props.bemBlocks.item('department')}
                dangerouslySetInnerHTML={{
                  __html: get(
                    props.result,
                    'highlight.department',
                    props.result._source.department
                  ),
                }}
              ></span>
              &nbsp;
              <span
                className={props.bemBlocks.item('number')}
                dangerouslySetInnerHTML={{
                  __html: get(
                    props.result,
                    'highlight.number',
                    props.result._source.number
                  ),
                }}
              ></span>
              &nbsp;
              <span
                className={props.bemBlocks.item('title')}
                dangerouslySetInnerHTML={{
                  __html: get(
                    props.result,
                    'highlight.title',
                    props.result._source.title
                  ),
                }}
              ></span>
            </h3>
          </a>
        </div>

        <CourseQuarterIndicator terms={props.result._source.terms} />
      </div>

      <div>
        <h4 className={'hit-subtitle'}>
          {props.result._source.school}
        </h4>

        <p
          className={props.bemBlocks.item('description')}
          dangerouslySetInnerHTML={{
            __html: get(
              props.result,
              'highlight.description',
              props.result._source.description
            ),
          }}
        ></p>

        <div className='hit-badges'>
          {
            pillData.map(pill => <Badge pill className='p-2 mr-3' variant='info'>
              {pill}
            </Badge>)
          }
        </div>
      </div>
    </div>
  )
};

export default CourseHitItem;
