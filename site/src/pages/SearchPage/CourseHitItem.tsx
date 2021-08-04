import React, { useState, useEffect } from 'react';
import { get } from 'lodash';
import CourseQuarterIndicator from './CourseQuarterIndicator';
import { RenderComponentType, HitItemProps } from 'searchkit';

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
  useEffect(() => {
    window.scrollTo(0, 0);
  }, [])

  return (
    <div>
      <div style={{ display: 'flex' }}>
        <div>
          <a href={'/course/' + props.result._id}>
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
        <h4 className={'course-department_unit'}>
          {props.result._source.department}&nbsp;･&nbsp;
          {props.result._source.units[0]} units
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
        {props.result._source.prerequisite_text !== '' && (
          <p>
            <b>Prerequisite: </b> {props.result._source.prerequisite_text}
          </p>
        )}

        <p className={'course-department_unit'}>
          {props.result._source.ge_text}
        </p>

        <br />
      </div>
    </div>
  )
};

export default CourseHitItem;
