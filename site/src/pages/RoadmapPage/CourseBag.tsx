import { useCoursebag } from '../../hooks/coursebag';
import Course from './Course';
import './Coursebag.scss';
import { Draggable } from 'react-beautiful-dnd';
const CourseBag = () => {
  const { coursebag, removeCourseFromBag } = useCoursebag();

  return (
    <div className="coursebag-container">
      {coursebag.length ? (
        <h3 className="coursebag-title">Saved Courses</h3>
      ) : (
        <p>No courses saved. Try searching for something!</p>
      )}
      <div>
        {coursebag.map((course, index) => {
          return (
            <Draggable draggableId={`coursebag-${course.id}-${index}`} key={`coursebag-${index}`} index={index}>
              {(provided) => (
                <div
                  ref={provided.innerRef}
                  {...provided.draggableProps}
                  {...provided.dragHandleProps}
                  style={{
                    cursor: 'grab',
                    ...provided.draggableProps.style,
                  }}
                >
                  <Course
                    {...course}
                    onDelete={() => {
                      removeCourseFromBag(course);
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
