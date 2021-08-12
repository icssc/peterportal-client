import React, { useState, useEffect } from 'react';
import { get } from 'lodash';
import { RenderComponentType, HitItemProps } from 'searchkit';
import { setActiveCourse } from '../../store/slices/roadmapSlice';
import { useAppDispatch } from '../../store/hooks';
import Course from './Course';
import { Draggable } from "react-beautiful-dnd";

import { CourseData } from '../../types/types';

interface CourseHitItemProps extends HitItemProps {
  index: number;
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
  return (
    <Draggable
      key={`search-course-${props.index}`}
      draggableId={`search-${props.result._source.id}-${props.index}`}
      index={props.index}
    >
      {(provided) => {
        return (
          <div
            ref={provided.innerRef}
            {...provided.draggableProps}
            {...provided.dragHandleProps}
            onMouseDown={() => { dispatch(setActiveCourse(props.result._source)) }}
          >
            <Course {...props.result._source} />
          </div>
        );
      }}
    </Draggable>
  )
};

export default CourseHitItem;
