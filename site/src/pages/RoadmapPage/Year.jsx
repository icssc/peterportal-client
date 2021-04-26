import React, { useState } from "react";
import "./Year.scss";
import { Button, Icon, Popup } from "semantic-ui-react";
import { Droppable } from "react-beautiful-dnd";
import Quarter from "./Quarter.jsx";

const Year = ({ index, startYear, courses, units, removeYear, state }) => {
  const [showContent, setShowContent] = useState(false);

  return (
    <div className="year">
      <Button
        className="accordion"
        onClick={() => {
          setShowContent(!showContent);
        }}
      >
        <span className="accordion-title">
          <span id="year-title">
            <Icon
              id="accordion-icon"
              name={showContent ? "triangle down" : "triangle right"}
            />
            <span id="year-number">Year {index} </span>
            <span id="year-range">
              ({startYear} - {startYear + 1})
            </span>
          </span>
          <span id="year-stats">
            <span id="course-count">{courses}</span> courses,{" "}
            <span id="unit-count">{units}</span> units
          </span>
        </span>
      </Button>
      <Popup
        content={
          <div>
            <Button className="year-settings-btn">Edit Year</Button>
            <Button
              className="year-settings-btn"
              id="remove-btn"
              onClick={() => {
                removeYear(index);
              }}
            >
              Remove
            </Button>
          </div>
        }
        className="year-settings-popup"
        on="click"
        trigger={<Button className="edit-btn" icon="ellipsis horizontal" />}
        position="bottom center"
      />
      {showContent && (
        <div className="accordion-content">
          {
            <Droppable droppableId={index + "-fall"} type="COURSE">
              {(provided) => {
                return (
                  <div
                    ref={provided.innerRef}
                    {...provided.droppableProps}
                    className="quarter"
                  >
                    <Quarter
                      startYear={startYear}
                      units={units}
                      provided={provided}
                      yearIndex={index}
                      state={state}
                      quarter={"fall"}
                    />
                  </div>
                );
              }}
            </Droppable>
          }
          {
            <Droppable droppableId={index + "-winter"} type="COURSE">
              {(provided) => {
                return (
                  <div
                    ref={provided.innerRef}
                    {...provided.droppableProps}
                    className="quarter"
                  >
                    <Quarter
                      startYear={startYear}
                      units={units}
                      provided={provided}
                      yearIndex={index}
                      state={state}
                      quarter={"winter"}
                    />
                  </div>
                );
              }}
            </Droppable>
          }
          {
            <Droppable droppableId={index + "-spring"} type="COURSE">
              {(provided) => {
                return (
                  <div
                    ref={provided.innerRef}
                    {...provided.droppableProps}
                    className="quarter"
                  >
                    <Quarter
                      startYear={startYear}
                      units={units}
                      provided={provided}
                      yearIndex={index}
                      state={state}
                      quarter={"spring"}
                    />
                  </div>
                );
              }}
            </Droppable>
          }
        </div>
      )}
    </div>
  );
};

export default Year;
