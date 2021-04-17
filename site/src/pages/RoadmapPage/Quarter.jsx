import React from 'react'
import "./Quarter.scss";
import { Draggable } from "react-beautiful-dnd";
import Course from "./Quarter.jsx";

const Quarter = ({ startYear, units, state, provided }) => {

  return (
    <div className="quarter">
      <h2 className="quarter-title">Fall {startYear}</h2>
      <div className="quarter-units">{units} units</div>
      {state["1-fall"]?.map((course, index) => {
        return (
          <Draggable
            key={course.id}
            draggableId={course.id}
            index={index}
          >
            {(provided) => {
              return (
                <div
                  ref={provided.innerRef}
                  {...provided.draggableProps}
                  {...provided.dragHandleProps}
                >
                  <Course 
                    course={course}
                  />
                </div>
              );
            }}
          </Draggable>
        );
      })}
      {provided.placeholder}
    </div>
  );
}

export default Quarter