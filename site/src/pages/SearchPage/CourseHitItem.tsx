import React, { useState, useEffect } from 'react';
import { useHistory } from 'react-router-dom';
import { get } from 'lodash';
import CourseQuarterIndicator from './CourseQuarterIndicator';
import { RenderComponentType, HitItemProps } from 'searchkit';
import Badge from 'react-bootstrap/Badge'

import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setCourse } from '../../store/slices/popupSlice';
import { CourseData } from '../../types/types';
import { getCourseTags } from '../../helpers/util';

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

  // data to be displayed in pills
  let pillData = getCourseTags(props.result._source);

  const onClickName = () => {
    // if click on a course that is already in popup
    if (activeCourse && props.result._source.id == activeCourse.id) {
      history.push(`/course/${activeCourse.id}`)
    }
    // click on new or different course than popup
    else {
      dispatch(setCourse(props.result._source))
    }
  }

  return (
    <div className='hit-item' style={{ cursor: 'pointer' }} onClick={onClickName}>
      <div style={{ display: 'flex' }}>
        <div>
          <h3>
            <span
              className={props.bemBlocks.item('department')}
              style={{ display: 'inline-block' }}
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
              style={{ display: 'inline-block' }}
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
              style={{ display: 'inline-block' }}
              dangerouslySetInnerHTML={{
                __html: get(
                  props.result,
                  'highlight.title',
                  props.result._source.title
                ),
              }}
            ></span>
          </h3>
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
