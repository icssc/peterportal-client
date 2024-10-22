import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { removeCourseFromBag } from '../../store/slices/roadmapSlice';
import Course from './Course';
import './Coursebag.scss';
import { Draggable } from 'react-beautiful-dnd';
const CourseBag = () => {
  const { coursebag } = useAppSelector((state) => state.roadmap);
  const dispatch = useAppDispatch();

  return (
    <div className="coursebag-container">
      <h3 className="coursebag-title">Course Bag</h3>
      <div style={{ height: '100%' }}>
        {coursebag.map((course, index) => {
          return (
            <Draggable draggableId={`coursebag-${course.id}-${index}`} key={`coursebag-${index}`} index={index}>
              {(provided) => (
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
                  <Course
                    {...course}
                    onDelete={() => {
                      dispatch(removeCourseFromBag(course));
                    }}
                  />
                </div>
              )}
            </Draggable>
          );
        })}
      </div>
    </div>
  );
};

export default CourseBag;
