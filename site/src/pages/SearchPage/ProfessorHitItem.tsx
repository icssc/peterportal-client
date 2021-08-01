import React, { useState, useEffect } from 'react';
import './Hit.scss';
import { get } from 'lodash';
import { RenderComponentType, HitItemProps } from 'searchkit';

import { ProfessorData } from 'src/types/types';

interface ProfessorHitItemProps extends HitItemProps {
  result: {
    _id: string;
    _index: string;
    _score: number;
    _source: ProfessorData;
    _type: string;
  }
}

const ProfessorHitItem: RenderComponentType<ProfessorHitItemProps> = (props: ProfessorHitItemProps) => {
  useEffect(() => {
    window.scrollTo(0, 0);
  }, [])

  return (
    <div style={{ display: 'flex', margin: '0 0 42px' }}>
      <a href={'/professor/' + props.result._id}><div style={{ marginRight: '16px', minWidth: '50px', maxWidth: '50px', height: '50px', borderRadius: '50px', background: '#74D1F6', display: 'flex', alignItems: 'center' }}>
        <h3 style={{ width: '100%', textAlign: 'center', color: 'white' }}>
          {props.result._source.name.split(' ').map((x: string) => x[0])}
        </h3>
      </div></a>
      <div style={{ width: '100%' }}>
        <a href={'/professor/' + props.result._id}>
          <h3>
            <span
              className={props.bemBlocks.item('prof_name')}
              dangerouslySetInnerHTML={{
                __html: get(
                  props.result,
                  'highlight.name',
                  props.result._source.name
                ),
              }}
            ></span>

          </h3>
        </a>
        <h4 className={'course-department_unit'}>
          {props.result._source.department}&nbsp;･&nbsp;
          {props.result._source.title}
        </h4>


        {props.result._source.course_history.length > 0 &&
          <p><b>Recently taught:&nbsp;</b>
            {props.result._source.course_history.map((item: string, index: number) => {
              return <span>
                {(index ? ', ' : '')}
                <a style={{ color: 'black' }} href={'/course/' + item.replace(/\s+/g, '')}>
                  {item}
                </a>
              </span>
            })}
          </p>
        }
      </div>
    </div>
  )
};


export default ProfessorHitItem;