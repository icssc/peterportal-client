import React, { FC, useState } from 'react';
import { Button, Form, Modal } from 'react-bootstrap';
import { PlannerYearData } from '../../../types/types';
import { quarterDisplayNames } from '../../../helpers/planner';
import { quarters, QuarterName } from '@peterportal/types';

interface YearPopupQuarter {
  id: QuarterName;
  checked?: boolean;
}

interface YearModalProps {
  placeholderName: string;
  placeholderYear: number;
  show: boolean;
  setShow: React.Dispatch<React.SetStateAction<boolean>>;
  type: 'add' | 'edit';
  saveHandler: (x: PlannerYearData) => void;
  currentQuarters: QuarterName[];
}

const quarterValues: (selectedQuarters: string[]) => YearPopupQuarter[] = (quarterIds: string[]) => {
  const base: YearPopupQuarter[] = quarters.map((n) => ({ id: n }));
  quarterIds.forEach((id) => {
    const quarter = base.find((q) => q.id === id)!;
    quarter.checked = true;
  });
  return base;
};

const YearModal: FC<YearModalProps> = (props) => {
  const { placeholderName, placeholderYear, show, setShow, type, saveHandler, currentQuarters } = props;
  const [validated, setValidated] = useState(false);

  const [name, setName] = useState(placeholderName);
  const [year, setYear] = useState(placeholderYear);

  const [quarters, setQuarters] = useState<YearPopupQuarter[]>(quarterValues(currentQuarters));
  const quarterCheckboxes = quarters.map((q, i) => {
    const handleClick = (i: number) => {
      const newQuarters = quarters.slice();
      newQuarters[i].checked = !newQuarters[i].checked;
      setQuarters(newQuarters);
    };
    return (
      <Form.Check
        key={q.id}
        type="checkbox"
        id={'quarter-checkbox-' + q.id}
        label={quarterDisplayNames[q.id]}
        value={q.id}
        // Prop must be assigned a value that is not undefined
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

  const saveYear = () => {
    if (name === '' || year < 1000 || year > 9999 || Number.isNaN(year)) {
      return setValidated(false);
    }

    setValidated(false);
    saveHandler({
      startYear: year,
      name: name.trim(),
      quarters: quarters.filter((q) => q.checked).map((q) => ({ name: q.id, courses: [] })),
    });
  };

  return (
    <Modal show={show} onShow={resetForm} onHide={handleHide} centered className="ppc-modal">
      <Modal.Header closeButton>
        <h2>{title}</h2>
      </Modal.Header>
      <Modal.Body>
        <Form noValidate validated={validated} className="ppc-modal-form">
          <Form.Group className="form-group">
            <Form.Label className="ppc-modal-form-label">Name</Form.Label>
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
          <Form.Group className="form-group">
            <Form.Label className="ppc-modal-form-label">Start Year</Form.Label>
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
          <Form.Group className="form-group">
            <Form.Label>Include Quarters</Form.Label>
            {quarterCheckboxes}
          </Form.Group>
        </Form>
        <Button variant="primary" onClick={saveYear}>
          {type === 'add' ? 'Add to Roadmap' : 'Save Changes'}
        </Button>
      </Modal.Body>
    </Modal>
  );
};

export default YearModal;
