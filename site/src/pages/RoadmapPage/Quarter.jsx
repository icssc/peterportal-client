import React from "react";
import "./Quarter.scss";
import { Draggable } from "react-beautiful-dnd";
import Course from "./Course.jsx";

const Quarter = ({ year, units, provided, yearIndex, quarter, state }) => {
  let quarterTitle = quarter.charAt(0).toUpperCase() + quarter.slice(1);
  return (
    <div className="quarter">
      <h2 className="quarter-title">
        {quarterTitle} {year}
      </h2>
      <div className="quarter-units">{units} units</div>
      {state &&
        state["year-plans"][yearIndex][yearIndex + "-" + quarter]?.map(
          (course, index) => {
            return (
              <Draggable key={course.id} draggableId={course.id} index={index}>
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
          }
        )}
      {provided && provided.placeholder}
    </div>
  );
};

export default Quarter;
