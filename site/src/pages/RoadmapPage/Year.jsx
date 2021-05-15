import React, { useState } from "react";
import "./Year.scss";
import { Button, Popover, Overlay } from "react-bootstrap";
import {
  CaretRightFill,
  CaretDownFill,
  ThreeDots,
} from "react-bootstrap-icons";
import { Droppable } from "react-beautiful-dnd";
import Quarter from "./Quarter.jsx";

const Year = ({ index, startYear, removeYear, state, plannerStats }) => {
  const [showContent, setShowContent] = useState(false);
  const [show, setShow] = useState(false);
  const [target, setTarget] = useState(null);

  const handleEditClick = (event) => {
    setShow(!show);
    setTarget(event.target);
  };

  const calculateYearStats = () => {
    let unitCount = 0;
    let courseCount = 0;
    let quarterKeys = Array.from(Object.keys(plannerStats[index]));
    for (let quarterKey of quarterKeys) {
      unitCount += plannerStats[index][quarterKey][0];
      courseCount += plannerStats[index][quarterKey][1];
    }
    return [unitCount, courseCount];
  };

  let yearStats = calculateYearStats();
  let unitCount = yearStats[0];
  let courseCount = yearStats[1];

  return (
    <div className="year">
      <div className="yearTitleBar">
        <Button
          variant="link"
          className="accordion"
          onClick={() => {
            setShowContent(!showContent);
          }}
        >
          <span className="accordion-title">
            <span id="year-title">
              {showContent ? (
                <CaretDownFill className="caret-icon" />
              ) : (
                <CaretRightFill className="caret-icon" />
              )}
              <span id="year-number">Year {index} </span>
              <span id="year-range">
                ({startYear} - {startYear + 1})
              </span>
            </span>
            <span id="year-stats">
              <span id="course-count">{courseCount}</span>{" "}
              {courseCount === 1 ? "course" : "courses"},{" "}
              <span id="unit-count">{unitCount}</span>{" "}
              {unitCount === 1 ? "unit" : "units"}
            </span>
          </span>
        </Button>
        <ThreeDots onClick={handleEditClick} className="edit-btn" />
        <Overlay show={show} target={target} placement="bottom">
          <Popover>
            <Popover.Content className="year-settings-popup">
              <div>
                <Button variant="light" className="year-settings-btn">
                  Edit Year
                </Button>
                <Button
                  variant="light"
                  className="year-settings-btn"
                  id="remove-btn"
                  onClick={() => {
                    removeYear(index);
                  }}
                >
                  Remove
                </Button>
              </div>
            </Popover.Content>
          </Popover>
        </Overlay>
      </div>
      {showContent && (
        <div className="accordion-content">
          {
            <Droppable droppableId={index + "-fall"} type="COURSE">
              {(provided) => {
                return (
                  <div ref={provided.innerRef} {...provided.droppableProps}>
                    <Quarter
                      year={startYear}
                      provided={provided}
                      yearIndex={index}
                      state={state}
                      quarter={"fall"}
                      plannerStats={plannerStats}
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
                  <div ref={provided.innerRef} {...provided.droppableProps}>
                    <Quarter
                      year={startYear + 1}
                      provided={provided}
                      yearIndex={index}
                      state={state}
                      quarter={"winter"}
                      plannerStats={plannerStats}
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
                  <div ref={provided.innerRef} {...provided.droppableProps}>
                    <Quarter
                      year={startYear + 1}
                      provided={provided}
                      yearIndex={index}
                      state={state}
                      quarter={"spring"}
                      plannerStats={plannerStats}
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
