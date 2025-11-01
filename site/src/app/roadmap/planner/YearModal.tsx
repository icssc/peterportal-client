import React, { FC, useState } from 'react';
import { Modal } from 'react-bootstrap';
import { PlannerYearData } from '../../../types/types';
import { quarterDisplayNames } from '../../../helpers/planner';
import { quarters, QuarterName } from '@peterportal/types';
import { Button, Box, Checkbox, FormControl, FormControlLabel, FormLabel, TextField } from '@mui/material';

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
      <FormControlLabel
        className="quarter-label"
        key={q.id}
        label={quarterDisplayNames[q.id]}
        control={
          <Checkbox
            className="quarter-checkbox"
            checked={q.checked ?? false}
            onChange={() => handleClick(i)}
            name={`quarter-checkbox-${q.id}`}
            value={q.id}
            aria-label={quarterDisplayNames[q.id]}
          />
        }
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
        <Box component="form" noValidate>
          <FormControl>
            <FormLabel>Name</FormLabel>
            <TextField
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
              error={validated}
              slotProps={{
                htmlInput: {
                  maxLength: 35,
                },
              }}
              placeholder={placeholderName}
            />
          </FormControl>

          <FormControl>
            <FormLabel>Start Year</FormLabel>
            <TextField
              required
              type="number"
              name="year"
              value={year}
              onChange={(e) => setYear(parseInt(e.target.value))}
              error={validated}
              slotProps={{
                htmlInput: {
                  min: 1000,
                  max: 9999,
                },
              }}
              placeholder={placeholderYear.toString()}
            />
          </FormControl>
          <FormControl>
            <FormLabel>Include Quarters</FormLabel>
            {quarterCheckboxes}
          </FormControl>

          <Button variant="contained" onClick={saveYear} disableElevation>
            {' '}
            {/* @todo: should be able to remove disableElevation and variant after conversion to MUI Modal */}
            {type === 'add' ? 'Add to Roadmap' : 'Save Changes'}
          </Button>
        </Box>
      </Modal.Body>
    </Modal>
  );
};

export default YearModal;
