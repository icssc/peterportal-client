import { FC } from 'react';
import { Draggable } from 'react-beautiful-dnd';
import { isMobile } from 'react-device-detect';
import { useAppDispatch } from '../../store/hooks';
import { setActiveCourse, setShowAddCourse } from '../../store/slices/roadmapSlice';
import Course from './Course';

import { CourseGQLData } from '../../types/types';

interface CourseHitItemProps extends CourseGQLData {
  index: number;
}

const CourseHitItem: FC<CourseHitItemProps> = (props: CourseHitItemProps) => {
  const dispatch = useAppDispatch();
  // do not make course draggable on mobile
  if (isMobile) {
    return (
      <div
        onMouseDown={() => {
          dispatch(setActiveCourse(props));
          dispatch(setShowAddCourse(true));
        }}
        // use inline style here so dnd can calculate size
        style={{ margin: ' 0rem 2rem 1rem 2rem' }}
      >
        <Course {...props} />
      </div>
    );
  }
  // course is draggable on desktop
  else {
    return (
      <Draggable
        key={`search-course-${props.index}`}
        draggableId={`search-${props.id}-${props.index}`}
        index={props.index}
      >
        {(provided) => {
          return (
            <div
              ref={provided.innerRef}
              {...provided.draggableProps}
              {...provided.dragHandleProps}
              style={{
                // use inline style here so dnd can calculate size
                margin: ' 0rem 2rem 1rem 2rem',
                cursor: 'grab',
                ...provided.draggableProps.style,
              }}
              onMouseDown={() => {
                dispatch(setActiveCourse(props));
              }}
            >
              <Course {...props} />
            </div>
          );
        }}
      </Draggable>
    );
  }
};

export default CourseHitItem;
