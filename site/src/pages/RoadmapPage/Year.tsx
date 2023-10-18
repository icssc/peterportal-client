import React, { FC, useState, useRef } from "react";
import "./Year.scss";
import { Button, Form, Popover, Overlay, Dropdown, DropdownButton } from "react-bootstrap";
import {
  CaretRightFill,
  CaretDownFill,
  ThreeDots,
} from "react-bootstrap-icons";
import Quarter from "./Quarter";
import { useAppDispatch } from '../../store/hooks';
import { addQuarter, editYear, editName, deleteYear, clearYear } from '../../store/slices/roadmapSlice';

import { PlannerYearData } from '../../types/types';

interface YearProps {
  yearIndex: number;
  data: PlannerYearData;
}

const Year: FC<YearProps> = ({ yearIndex, data }) => {
  const dispatch = useAppDispatch();
  const [showContent, setShowContent] = useState(true);
  const [show, setShow] = useState(false);
  const [showAddQuarter, setShowAddQuarter] = useState(false);
  const [showEditYear, setShowEditYear] = useState(false);
  const [target, setTarget] = useState<any>(null!);
  const [addQuarterTarget, setAddQuarterTarget] = useState<any>(null!);
  const [editYearTarget, setEditYearTarget] = useState<any>(null!);
  const [placeholderYear, setPlaceholderYear] = useState(data.startYear);
  const [placeholderName, setPlaceholderName] = useState(data.name);

  const handleEditClick = (event: React.MouseEvent) => {
    if (showAddQuarter) {
      /* hide both overlays */
      setShowAddQuarter(!showAddQuarter);
      setShow(!show);
    } else if (showEditYear) {
      setShowEditYear(!showEditYear);
      setShow(!show);
    } else {
      setShow(!show);
      setTarget(event.target);
    }
  };

  const handleShowAddQuarterClick = (event: React.MouseEvent) => {
    setShowEditYear(false); // hide any other currently displayed menu bar options
    setShowAddQuarter(!showAddQuarter);
    setAddQuarterTarget(event.target);
  }

  const handleAddQuarterClick = (year: number, quarter: string) => {
    dispatch(addQuarter({ startYear: year, quarterData: { name: quarter, courses: [] } }));
  }

  const handleEditYearClick = (event: React.MouseEvent) => {
    setShowAddQuarter(false);           // hide any other currently displayed menu bar options
    setPlaceholderYear(data.startYear); // set default year to current year
    setShowEditYear(!showEditYear);
    setEditYearTarget(event.target);
  }

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
              <span id="year-number">{data.name} </span>
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
                <Button disabled={!(data.quarters && data.quarters.length < 6)} onClick={handleShowAddQuarterClick} variant="light" className="year-settings-btn">
                  Add Quarter
                </Button>
                <Button onClick={handleEditYearClick} variant="light" className="year-settings-btn">
                  Edit Year
                </Button>
                <Button
                  variant="light"
                  className="year-settings-btn"
                  id="clear-btn"
                  onClick={() => {
                    dispatch(clearYear({
                      yearIndex: yearIndex
                    }));
                  }}
                >
                  Clear
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
        <Overlay show={showAddQuarter && data.quarters && data.quarters.length < 6} target={addQuarterTarget} placement="left">
          <Popover id={`add-quarter-menu-${yearIndex}`}>
            <Popover.Content>
              <div>
                {!data.quarters.map(quarter => quarter.name).includes("fall") && <Button onClick={() => handleAddQuarterClick(data.startYear, "fall")} variant="light" className="year-settings-btn">Fall</Button>}
                {!data.quarters.map(quarter => quarter.name).includes("winter") && <Button onClick={() => handleAddQuarterClick(data.startYear, "winter")} variant="light" className="year-settings-btn">Winter</Button>}
                {!data.quarters.map(quarter => quarter.name).includes("spring") && <Button onClick={() => handleAddQuarterClick(data.startYear, "spring")} variant="light" className="year-settings-btn">Spring</Button>}
                {!data.quarters.map(quarter => quarter.name).includes("summer I") && <Button onClick={() => handleAddQuarterClick(data.startYear, "summer I")} variant="light" className="year-settings-btn">Summer I</Button>}
                {!data.quarters.map(quarter => quarter.name).includes("summer II") && <Button onClick={() => handleAddQuarterClick(data.startYear, "summer II")} variant="light" className="year-settings-btn">Summer II</Button>}
                {!data.quarters.map(quarter => quarter.name).includes("summer 10 Week") && <Button onClick={() => handleAddQuarterClick(data.startYear, "summer 10 Week")} variant="light" className="year-settings-btn">Summer 10 Week</Button>}
              </div>
            </Popover.Content>
          </Popover>
        </Overlay>
        <Overlay show={showEditYear} target={editYearTarget} placement="left">
          <Popover id={`edit-year-menu-${yearIndex}`}>
            <Popover.Content>
              <Form>
                <Form.Group>
                  <Form.Label className="edit-year-form-label">
                    Name
                  </Form.Label>
                  <Form.Control
                    type="text"
                    name="name"
                    value={placeholderName}
                    onChange={(e) => {
                      setPlaceholderName(e.target.value);
                    }}
                    onKeyDown={(e: React.KeyboardEvent) => {
                      // prevent submitting form (reloads the page)
                      if (e.key === 'Enter') {
                        e.preventDefault();
                      }
                    }}
                    placeholder={placeholderName}
                  ></Form.Control>
                </Form.Group>
                <Form.Group>
                  <Form.Label className="edit-year-form-label">
                    Start Year
                  </Form.Label>
                  <Form.Control
                    type="number"
                    name="year"
                    value={placeholderYear}
                    onChange={(e) => {
                      setPlaceholderYear(parseInt(e.target.value));
                    }}
                    onKeyDown={(e: React.KeyboardEvent) => {
                      // prevent submitting form (reloads the page)
                      if (e.key === 'Enter') {
                        e.preventDefault();
                      }
                    }}
                    min={1000}
                    max={9999}
                    placeholder={placeholderYear.toString()}
                  ></Form.Control>
                </Form.Group>
                <Button
                  className="edit-year-popup-btn"
                  onClick={() => {
                    setPlaceholderName(placeholderName.trim());
                    setShowEditYear(!showEditYear);
                    setShow(!show);
                    if (placeholderYear !== data.startYear) {
                      dispatch(editYear({ startYear: placeholderYear, index: yearIndex }));
                    }
                    if (placeholderName !== data.name) {
                      dispatch(editName({ name: placeholderName.trim(), index: yearIndex }));
                    }
                  }}
                >
                  Confirm
                </Button>
              </Form>
            </Popover.Content>
          </Popover>
        </Overlay>
      </div>
      {showContent && (
        <div className="year-accordion-content">
          {
            data.quarters.map((quarter, quarterIndex) => {
              return <Quarter
                key={`year-quarter-${quarterIndex}`}
                year={data.startYear + (quarterIndex == 0 ? 0 : 1)}
                yearIndex={yearIndex}
                quarterIndex={quarterIndex}
                data={quarter}
              />
            })
          }

          {/* render blank, non-functional quarters to ensure there are 3 per row */}
          {data.quarters.length > 3 && data.quarters.length < 6 && (
            [undefined, undefined].slice(data.quarters.length - 4).map(() => {
              return <div className="empty-quarter"></div>
            })
          )
          }
        </div>
      )}
    </div>
  );
};

export default Year;
