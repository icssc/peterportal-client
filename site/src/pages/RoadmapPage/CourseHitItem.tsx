import React, { FC } from 'react';
import { setActiveCourse, setShowAddCourse, setShowSearch } from '../../store/slices/roadmapSlice';
import { useAppDispatch } from '../../store/hooks';
import Course from './Course';
import { Draggable } from "react-beautiful-dnd";
import { isMobile, isBrowser } from 'react-device-detect';

import { CourseGQLData } from '../../types/types';

interface CourseHitItemProps extends CourseGQLData {
  index: number;
}

const CourseHitItem: FC<CourseHitItemProps> = (props: CourseHitItemProps) => {
  const dispatch = useAppDispatch();
  // do not make course draggable on mobile
  if (isMobile) {
    return <div onMouseDown={() => {
      dispatch(setActiveCourse(props));
      dispatch(setShowAddCourse(true));
      // also hide the search bar to view the roadmap
      dispatch(setShowSearch(false));
    }}
      // use inline style here so dnd can calculate size
      style={{ margin: ' 0rem 2rem 1rem 2rem' }}>
      <Course {...props} />
    </div>
  }
  // course is draggable on desktop
  else {
    return <Draggable
      key={`search-course-${props.index}`}
      draggableId={`search-${props.id}-${props.index}`}
      index={props.index}
    >
      {(provided, snapshot) => {
        return (
          <div
            ref={provided.innerRef}
            {...provided.draggableProps}
            {...provided.dragHandleProps}
            style={{
              // use inline style here so dnd can calculate size
              margin: ' 0rem 2rem 1rem 2rem',
              ...provided.draggableProps.style
            }}
            onMouseDown={() => { dispatch(setActiveCourse(props)) }}
          >
            <Course {...props} />
          </div>
        );
      }}
    </Draggable>
  }
};

export default CourseHitItem;
