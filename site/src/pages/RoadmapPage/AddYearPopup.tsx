import React, { FC, useState, useContext } from 'react';
import './AddYearPopup.scss';
import { PlusCircleFill } from 'react-bootstrap-icons';
import { Button, Form, Modal } from 'react-bootstrap';
import { addYear } from '../../store/slices/roadmapSlice';
import { useAppDispatch } from '../../store/hooks';
import ThemeContext from '../../style/theme-context';

interface AddYearPopupProps {
  placeholderName: string;
  placeholderYear: number;
}

interface YearPopupQuarter {
  id: string;
  name: string;
  checked?: boolean;
}

const AddYearPopup: FC<AddYearPopupProps> = ({ placeholderName, placeholderYear }) => {
  const { darkMode } = useContext(ThemeContext);
  const [showModal, setShowModal] = useState(false);

  const dispatch = useAppDispatch();
  const [validated, setValidated] = useState(false);

  const [name, setName] = useState(placeholderName);
  const [year, setYear] = useState(placeholderYear);

  const [quarters, setQuarters] = useState<YearPopupQuarter[]>([
    { id: 'fall', name: 'Fall', checked: true },
    { id: 'winter', name: 'Winter', checked: true },
    { id: 'spring', name: 'Spring', checked: true },
    { id: 'summer 1', name: 'Summer 1' },
    { id: 'summer 2', name: 'Summer 2' },
    { id: 'summer 10 Week', name: 'Summer 10 Week' },
  ]);
  const quarterCheckboxes = quarters.map((q, i) => {
    const handleClick = (i: number) => {
      quarters[i].checked = !quarters[i].checked;
      setQuarters(quarters.slice());
    };
    return (
      <Form.Check
        key={q.id}
        type="checkbox"
        id={'quarter-checkbox-' + q.id}
        label={q.name}
        value={q.id}
        checked={q.checked ?? false}
        onInput={() => handleClick(i)}
      />
    );
  });

  return (
    <div>
      <Modal show={showModal} onHide={() => setShowModal(false)} centered className="planner-year-modal">
        <Modal.Header closeButton>
          <h2>Add Year</h2>
        </Modal.Header>
        <Modal.Body>
          <Form noValidate validated={validated} className="add-year-form">
            <Form.Group>
              <Form.Label className="add-year-form-label">Name</Form.Label>
              <Form.Control
                required
                type="text"
                name="name"
                value={name}
                onChange={(e) => setName(e.target.value)}
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
            <Form.Group>
              <Form.Label>Include Quarters</Form.Label>
              {quarterCheckboxes}
            </Form.Group>
          </Form>
          <Button
            variant="primary"
            // className="popup-btn"
            onClick={() => {
              if (name === '' || year < 1000 || year > 9999 || Number.isNaN(year)) {
                setValidated(false);
                return;
              }

              setValidated(false);
              setShowModal(false);
              const yearData = {
                startYear: year,
                name: name.trim(),
                quarters: quarters.filter((q) => q.checked).map((q) => ({ name: q.name, courses: [] })),
              };
              dispatch(addYear({ yearData }));
              setYear(placeholderYear);
              setName(placeholderName);
            }}
          >
            Add to Roadmap
          </Button>
        </Modal.Body>
      </Modal>
      <Button variant={darkMode ? 'dark' : 'light'} className="add-year-btn" onClick={() => setShowModal(true)}>
        <PlusCircleFill className="add-year-icon" />
        <div className="add-year-text">Add year</div>
      </Button>
    </div>
  );
};

export default AddYearPopup;
