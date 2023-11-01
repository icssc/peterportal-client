import { FC, useState } from "react";
import "./Quarter.scss";
import { Draggable, Droppable } from "react-beautiful-dnd";
import Course from "./Course";

import { useAppDispatch, useAppSelector } from "../../store/hooks";
import { deleteQuarter, clearQuarter, deleteCourse } from '../../store/slices/roadmapSlice';
import { PlannerQuarterData } from '../../types/types';
import { Button, Overlay, Popover } from "react-bootstrap";
import {
  ThreeDots
} from "react-bootstrap-icons";

interface QuarterProps {
  year: number;
  yearIndex: number;
  quarterIndex: number;
  data: PlannerQuarterData
}

const Quarter: FC<QuarterProps> = ({ year, yearIndex, quarterIndex, data }) => {
  const dispatch = useAppDispatch();
  const quarterTitle = data.name.charAt(0).toUpperCase() + data.name.slice(1);
  const invalidCourses = useAppSelector(state => state.roadmap.invalidCourses);

  const [showQuarterMenu, setShowQuarterMenu] = useState(false);
  const [threeDotMenuTarget, setThreeDotMenuTarget] = useState<HTMLElement | null>(null);

  const handleQuarterMenuClick = (event: React.MouseEvent) => {
    setShowQuarterMenu(!showQuarterMenu);
    setThreeDotMenuTarget(event.target as HTMLElement);
  }

  const calculateQuarterStats = () => {
    let unitCount = 0;
    let courseCount = 0;
    data.courses.forEach(course => {
      unitCount += course.units[0];
      courseCount += 1;
    })
    return [unitCount, courseCount];
  };

  const unitCount = calculateQuarterStats()[0];

  const renderCourses = () => {
    return data.courses.map((course, index) => {
      return <Draggable key={`quarter-course-${index}`} draggableId={`${yearIndex}-${quarterIndex}-${course.id}-${index}`} index={index}>
        {(provided) => {
          let requiredCourses: string[] = null!;
          // if this is an invalid course, set the required courses
          invalidCourses.forEach(ic => {
            const loc = ic.location;
            if (loc.courseIndex == index && loc.quarterIndex == quarterIndex && loc.yearIndex == yearIndex) {
              requiredCourses = ic.required;
            }
          });

          const onDelete = () => {
            dispatch(deleteCourse({
              yearIndex,
              quarterIndex,
              courseIndex: index,
            }));
          };

          return (
            <div
              ref={provided.innerRef}
              {...provided.draggableProps}
              {...provided.dragHandleProps}
              style={{
                margin: '0rem 2rem 1rem 2rem',
                ...provided.draggableProps.style
              }}
            >
              <Course key={course.id} {...course}
                requiredCourses={requiredCourses}
                onDelete={onDelete} />
            </div>
          );
        }}
      </Draggable>
    })
  }

  return <div className="quarter">
    <span className="quarter-header">
      <h2 className="quarter-title">
        {quarterTitle} {year}
      </h2>
      <ThreeDots onClick={handleQuarterMenuClick} className="edit-btn" />
      <Overlay show={showQuarterMenu} target={threeDotMenuTarget} placement="bottom">
        <Popover id={`quarter-menu-${yearIndex}-${quarterIndex}`}>
          <Popover.Content>
            <div>
              <Button variant="light" className="quarter-menu-btn red-menu-btn" onClick={() => dispatch(clearQuarter({ yearIndex: yearIndex, quarterIndex: quarterIndex }))}>
                Clear
              </Button>
              <Button variant="light" className="quarter-menu-btn red-menu-btn" onClick={() => {
                dispatch(deleteQuarter({ yearIndex: yearIndex, quarterIndex: quarterIndex }));
                setShowQuarterMenu(false)
              }}>
                Delete
              </Button>
            </div>
          </Popover.Content>
        </Popover>
      </Overlay>
    </span>
    <div className="quarter-units">
      {unitCount} {unitCount === 1 ? "unit" : "units"}
    </div>
    <Droppable droppableId={yearIndex + "-" + quarterIndex} type="COURSE">
      {(provided) => {
        return (
          <div
            ref={provided.innerRef}
            {...provided.droppableProps}
            style={{ paddingBottom: '1rem' }}>
            {renderCourses()}
            {provided.placeholder}
          </div>
        );
      }}
    </Droppable>
  </div>
};

export default Quarter;
