import React from "react";
import "./Quarter.scss";
import { Draggable } from "react-beautiful-dnd";
import Course from "./Course.jsx";

const Quarter = ({
  year,
  provided,
  yearIndex,
  quarter,
  state,
  plannerStats,
}) => {
  let quarterTitle = quarter.charAt(0).toUpperCase() + quarter.slice(1);
  let quarterContents = null;
  let quarterKey = yearIndex + "-" + quarter;
  if (quarterKey in state["year-plans"][yearIndex]) {
    quarterContents = state["year-plans"][yearIndex][quarterKey];
  }

  const calculateQuarterStats = () => {
    let unitCount = plannerStats[yearIndex][quarter][0];
    let courseCount = plannerStats[yearIndex][quarter][1];
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
      {state &&
        quarterContents?.map((course, index) => {
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
        })}
      {provided && provided.placeholder}
    </div>
  );
};

export default Quarter;
