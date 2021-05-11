import React, { useState, useRef } from "react";
import "./AddYearPopup.scss";
import { PlusCircleFill } from "react-bootstrap-icons";
import { Button, Form, Popover, Overlay } from "react-bootstrap";

function AddYearPopup({ addYearToPlanner }) {
  const placeholderYear = new Date().getFullYear();
  const [year, setYear] = useState(placeholderYear);
  const [show, setShow] = useState(false);
  const [target, setTarget] = useState(null);

  const handleClick = (event) => {
    setShow(!show);
    setTarget(event.target);
  };

  return (
    <div>
      <Button variant="light" className="add-year-btn" onClick={handleClick}>
        <PlusCircleFill className="add-year-icon" />
        Add year
      </Button>
      <Overlay show={show} target={target} placement="bottom">
        <Popover>
          <Popover.Content>
            <Form>
              <Form.Group>
                <Form.Label className="add-year-form-label">
                  Start Year
                </Form.Label>
                <Form.Control
                  type="number"
                  name="year"
                  value={year}
                  onChange={(e) => {
                    setYear(e.target.value);
                  }}
                  min={1000}
                  max={9999}
                  placeholder={placeholderYear.toString()}
                ></Form.Control>
              </Form.Group>
              <Button
                className="popup-btn"
                onClick={() => {
                  setShow(!show);
                  addYearToPlanner(year);
                  setYear(placeholderYear);
                }}
              >
                Add Year
              </Button>
            </Form>
          </Popover.Content>
        </Popover>
      </Overlay>
    </div>
  );
}

export default AddYearPopup;
