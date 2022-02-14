import React, { FC } from "react";
import "./Quarter.scss";
import { Draggable, DroppableProvided } from "react-beautiful-dnd";
import Course from "./Course";

import { PlannerQuarterData } from '../../types/types';

interface QuarterProps {
  year: number;
  provided: DroppableProvided;
  yearIndex: number;
  quarterIndex: number;
  data: PlannerQuarterData
}

const Quarter: FC<QuarterProps> = ({ year, provided, yearIndex, quarterIndex, data }) => {
  let quarterTitle = data.name.charAt(0).toUpperCase() + data.name.slice(1);

  const calculateQuarterStats = () => {
    let unitCount = 0;
    let courseCount = 0;
    data.courses.forEach(course => {
      unitCount += course.units[0];
      courseCount += 1;
    })
    return [unitCount, courseCount];
  };

  let unitCount = calculateQuarterStats()[0];

  return (
    <div className="quarter">
      <h2 className="quarter-title">
        {quarterTitle} {year}
      </h2>
      <div className="quarter-units">
        {unitCount} {unitCount === 1 ? "unit" : "units"}
      </div>
      {data.courses.map((course, index) => {
        return (
          <Draggable key={`quarter-course-${index}`} draggableId={`${yearIndex}-${quarterIndex}-${course.id}-${index}`} index={index}>
            {(provided) => {
              return (
                <div
                  ref={provided.innerRef}
                  {...provided.draggableProps}
                  {...provided.dragHandleProps}
                >
                  <Course key={course.id} {...course} />
                </div>
              );
            }}
          </Draggable>
        );
      })}
      {provided && provided.placeholder}
    </div>
  );
};

export default Quarter;
