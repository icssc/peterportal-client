import { FC } from 'react';
import { setActiveCourse, setShowAddCourse, setShowSearch } from '../../store/slices/roadmapSlice';
import { useAppDispatch } from '../../store/hooks';
import Course from './Course';
import { Draggable } from 'react-beautiful-dnd';

import { CourseGQLData } from '../../types/types';
import { useIsMobile } from '../../helpers/util';

interface CourseHitItemProps extends CourseGQLData {
  index: number;
  unmatchedPrerequisites: string[];
}

const CourseHitItem: FC<CourseHitItemProps> = (props: CourseHitItemProps) => {
  const dispatch = useAppDispatch();
  const isMobile = useIsMobile();
  // do not make course draggable on mobile
  const onMobileMouseDown = () => {
    dispatch(setActiveCourse(props));
    dispatch(setShowAddCourse(true));
    // also hide the search bar to view the roadmap
    dispatch(setShowSearch(false));
  };
  const onMobileKeyDown = (e: React.KeyboardEvent<HTMLDivElement>) => {
    if (e.key === 'Enter') {
      onMobileMouseDown();
    }
  };
  if (isMobile) {
    return (
      <div
        tabIndex={0}
        role="button"
        onMouseDown={onMobileMouseDown}
        onKeyDown={onMobileKeyDown}
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
