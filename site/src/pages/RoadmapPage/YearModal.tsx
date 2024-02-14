import React, { FC, useState } from 'react';
import { Button, Form, Modal } from 'react-bootstrap';
import { PlannerYearData } from '../../types/types';
import './YearModal.scss';

interface YearPopupQuarter {
  id: string;
  name: string;
  checked?: boolean;
}

interface CourseYearModalProps {
  placeholderName: string;
  placeholderYear: number;
  show: boolean;
  setShow: React.Dispatch<React.SetStateAction<boolean>>;
  type: 'add' | 'edit';
  saveHandler: (x: PlannerYearData) => void;
  currentQuarters: string[];
}

const CourseYearModal: FC<CourseYearModalProps> = (props) => {
  const { placeholderName, placeholderYear, show, setShow, type, saveHandler, currentQuarters } = props;
  const [validated, setValidated] = useState(false);

  const [name, setName] = useState(placeholderName);
  const [year, setYear] = useState(placeholderYear);

  const quarterValues: (selectedNames: string[]) => YearPopupQuarter[] = (data: string[]) => {
    const base: YearPopupQuarter[] = [
      { id: 'fall', name: 'Fall' },
      { id: 'winter', name: 'Winter' },
      { id: 'spring', name: 'Spring' },
      { id: 'summer 1', name: 'Summer 1' },
      { id: 'summer 2', name: 'Summer 2' },
      { id: 'summer 10 Week', name: 'Summer 10 Week' },
    ];
    data.forEach((name) => {
      const match = base.find((q) => q.id === name);
      if (match) match.checked = true;
    });
    return base;
  };

  const [quarters, setQuarters] = useState<YearPopupQuarter[]>(quarterValues(currentQuarters));
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
        onChange={() => handleClick(i)}
      />
    );
  });

  const title = type === 'add' ? 'Add Year' : `Editing "${placeholderName}"`;

  const resetForm = () => {
    setName(placeholderName);
    setYear(placeholderYear);
    setQuarters(quarterValues(currentQuarters));
  };

  const handleHide = () => {
    resetForm();
    setShow(false);
  };

  return (
    <Modal show={show} onShow={resetForm} onHide={handleHide} centered className="planner-year-modal">
      <Modal.Header closeButton>
        <h2>{title}</h2>
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
          onClick={() => {
            if (name === '' || year < 1000 || year > 9999 || Number.isNaN(year)) {
              return setValidated(false);
            }

            setValidated(false);
            setShow(false);
            saveHandler({
              startYear: year,
              name: name.trim(),
              quarters: quarters.filter((q) => q.checked).map((q) => ({ name: q.id, courses: [] })),
            });
            setYear(placeholderYear);
            setName(placeholderName);
          }}
        >
          {type === 'add' ? 'Add to Roadmap' : 'Save Changes'}
        </Button>
      </Modal.Body>
    </Modal>
  );
};

export default CourseYearModal;
