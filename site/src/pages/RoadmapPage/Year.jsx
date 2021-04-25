import React, { useState } from "react";
import "./Year.scss";
import { Button, Icon, Popup } from "semantic-ui-react";
import { Draggable, Droppable } from "react-beautiful-dnd";
import Quarter from "./Quarter.jsx";
import Course from "./Course.jsx";

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
          {index === 1 && (
            <Droppable droppableId={index + "-fall"} type="COURSE">
              {(provided) => {
                return (
                  <div
                    ref={provided.innerRef}
                    {...provided.droppableProps}
                    className="quarter"
                  >
                    <h2 className="quarter-title">Fall {startYear}</h2>
                    <div className="quarter-units">{units} units</div>
                    {state["year-plans"][index][index + "-fall"]?.map(
                      (course, index) => {
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
                                  <Course {...course} />
                                </div>
                              );
                            }}
                          </Draggable>
                        );
                      }
                    )}
                    {provided.placeholder}
                  </div>
                );
              }}
            </Droppable>
          )}
          {index === 1 && (
            <Droppable droppableId={index + "-winter"} type="COURSE">
              {(provided) => {
                return (
                  <div
                    ref={provided.innerRef}
                    {...provided.droppableProps}
                    className="quarter"
                  >
                    <h2 className="quarter-title">Winter {startYear + 1}</h2>
                    <div className="quarter-units">{units} units</div>
                    {state["year-plans"][index][index + "-winter"]?.map(
                      (course, index) => {
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
                                  <Course {...course} />
                                </div>
                              );
                            }}
                          </Draggable>
                        );
                      }
                    )}
                    {provided.placeholder}
                  </div>
                );
              }}
            </Droppable>
          )}
          {index === 1 && (
            <Droppable droppableId={index + "-spring"} type="COURSE">
              {(provided) => {
                return (
                  <div
                    ref={provided.innerRef}
                    {...provided.droppableProps}
                    className="quarter"
                  >
                    <h2 className="quarter-title">Spring {startYear + 1}</h2>
                    <div className="quarter-units">{units} units</div>
                    {state["year-plans"][index][index + "-spring"]?.map(
                      (course, index) => {
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
                                  <Course {...course} />
                                </div>
                              );
                            }}
                          </Draggable>
                        );
                      }
                    )}
                    {provided.placeholder}
                  </div>
                );
              }}
            </Droppable>
          )}
          {index === 2 && (
            <Droppable droppableId={index + "-fall"} type="COURSE">
              {(provided) => {
                return (
                  <div
                    ref={provided.innerRef}
                    {...provided.droppableProps}
                    className="quarter"
                  >
                    <h2 className="quarter-title">Fall {startYear}</h2>
                    <div className="quarter-units">{units} units</div>
                    {state["year-plans"][index][index + "-fall"]?.map(
                      (course, index) => {
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
                                  <Course {...course} />
                                </div>
                              );
                            }}
                          </Draggable>
                        );
                      }
                    )}
                    {provided.placeholder}
                  </div>
                );
              }}
            </Droppable>
          )}
        </div>
      )}
    </div>
  );
};

export default Year;
