import React, { FC, useState, useEffect } from 'react';
import './AddYearPopup.scss';
import { PlusCircleFill } from 'react-bootstrap-icons';
import { Button, Form, Popover, OverlayTrigger } from 'react-bootstrap';
import { addYear } from '../../store/slices/roadmapSlice';
import { useAppDispatch } from '../../store/hooks';

interface AddYearPopupProps {
  placeholderName: string;
  placeholderYear: number;
}

const AddYearPopup: FC<AddYearPopupProps> = ({ placeholderName, placeholderYear }) => {
  const dispatch = useAppDispatch();
  const [name, setName] = useState(placeholderName);
  const [year, setYear] = useState(placeholderYear);
  const [validated, setValidated] = useState(false);
  const [show, setShow] = useState(false);

  useEffect(() => {
    setYear(placeholderYear);
    setName(placeholderName);
  }, [placeholderYear, placeholderName]);

  const overlay = (
    <Popover id="add-year-popover">
      <Popover.Content>
        <Form noValidate validated={validated} className="add-year-form">
          <Form.Group>
            <Form.Label className="add-year-form-label">Name</Form.Label>
            <Form.Control
              required
              type="text"
              name="name"
              value={name}
              onChange={(e) => {
                setName(e.target.value);
              }}
              onKeyDown={(e: React.KeyboardEvent) => {
                // prevent submitting form (reloads the page)
                if (e.key === 'Enter') {
                  e.preventDefault();
                }
              }}
              maxLength={35}
              placeholder={placeholderName}
            ></Form.Control>
          </Form.Group>
          <Form.Group>
            <Form.Label className="add-year-form-label">Start Year</Form.Label>
            <Form.Control
              required
              type="number"
              name="year"
              value={year}
              onChange={(e) => {
                setYear(parseInt(e.target.value));
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
            className="popup-btn"
            onClick={() => {
              if (name === '' || year < 1000 || year > 9999 || Number.isNaN(year)) {
                setValidated(true);
                return;
              }

              setValidated(false);
              setShow(!show);
              dispatch(
                addYear({
                  yearData: {
                    startYear: year,
                    name: name.trim(),
                    quarters: ['fall', 'winter', 'spring'].map((quarter) => {
                      return { name: quarter, courses: [] };
                    }),
                  },
                }),
              );
              setYear(placeholderYear);
              setName(placeholderName);
            }}
          >
            Add Year
          </Button>
        </Form>
      </Popover.Content>
    </Popover>
  );

  return (
    <div>
      <OverlayTrigger placement="top" overlay={overlay} trigger="click" rootClose onToggle={setShow} show={show}>
        <Button variant="light" className="add-year-btn">
          <PlusCircleFill className="add-year-icon" />
          <div className="add-year-text">Add year</div>
        </Button>
      </OverlayTrigger>
    </div>
  );
};

export default AddYearPopup;
