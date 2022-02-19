import React, { FC, useState, useRef } from "react";
import "./Year.scss";
import { Button, Popover, Overlay } from "react-bootstrap";
import {
  CaretRightFill,
  CaretDownFill,
  ThreeDots,
} from "react-bootstrap-icons";
import { Droppable } from "react-beautiful-dnd";
import Quarter from "./Quarter";
import { useAppDispatch } from '../../store/hooks';
import { deleteYear } from '../../store/slices/roadmapSlice';

import { PlannerYearData } from '../../types/types';

interface YearProps {
  yearIndex: number;
  data: PlannerYearData;
}

const Year: FC<YearProps> = ({ yearIndex, data }) => {
  const dispatch = useAppDispatch();
  const [showContent, setShowContent] = useState(true);
  const [show, setShow] = useState(false);
  const [target, setTarget] = useState<any>(null!);

  const handleEditClick = (event: React.MouseEvent) => {
    setShow(!show);
    setTarget(event.target);
  };

  const calculateYearStats = () => {
    let unitCount = 0;
    let courseCount = 0;
    data.quarters.forEach(quarter => {
      quarter.courses.forEach(course => {
        unitCount += course.units[0];
        courseCount += 1;
      })
    })
    return { unitCount, courseCount };
  };

  let { unitCount, courseCount } = calculateYearStats();

  return (
    <div className="year">
      <div className="yearTitleBar">
        <Button
          variant="link"
          className="year-accordion"
          onClick={() => {
            setShowContent(!showContent);
          }}
        >
          <span className="year-accordion-title">
            <span id="year-title">
              {showContent ? (
                <CaretDownFill className="caret-icon" />
              ) : (
                <CaretRightFill className="caret-icon" />
              )}
              <span id="year-number">Year {yearIndex + 1} </span>
              <span id="year-range">
                ({data.startYear} - {data.startYear + 1})
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
          <Popover id={`year-menu-${yearIndex}`}>
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
                    dispatch(deleteYear({
                      yearIndex: yearIndex
                    }));
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
        <div className="year-accordion-content">
          {
            data.quarters.map((quarter, quarterIndex) => {
              return <Droppable key={`year-quarter-${quarterIndex}`} droppableId={yearIndex + "-" + quarterIndex} type="COURSE">
                {(provided) => {
                  return (
                    <div ref={provided.innerRef} {...provided.droppableProps} style={{ flex: 1 }}>
                      <Quarter
                        year={data.startYear + (quarterIndex == 0 ? 0 : 1)}
                        provided={provided}
                        yearIndex={yearIndex}
                        quarterIndex={quarterIndex}
                        data={quarter}
                      />
                    </div>
                  );
                }}
              </Droppable>
            })
          }
        </div>
      )}
    </div>
  );
};

export default Year;
