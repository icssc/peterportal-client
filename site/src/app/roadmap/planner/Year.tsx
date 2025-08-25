'use client';
import { FC, useRef, useState } from 'react';
import './Year.scss';
import { Button, Modal } from 'react-bootstrap';
import Quarter from './Quarter';
import { useAppDispatch } from '../../../store/hooks';
import { addQuarter, editYear, editName, deleteYear, deleteQuarter } from '../../../store/slices/roadmapSlice';
import { pluralize } from '../../../helpers/util';

import { PlannerYearData } from '../../../types/types';
import EditYearModal from './YearModal';

import { Box, Card, Collapse, Divider, FormControl, IconButton } from '@mui/material';
import EditIcon from '@mui/icons-material/Edit';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import { ExpandMore } from '../../../component/ExpandMore/ExpandMore';

interface YearTitleProps {
  year: PlannerYearData;
  index: number;
}
const YearTitle = ({ year, index }: YearTitleProps) => {
  return (
    <span className="year-title">
      {year.name ? (
        <span className="year-number">{year.name} </span>
      ) : (
        <span className="year-number">Year {index + 1} </span>
      )}
      <span className="year-range">
        ({year.startYear}-{year.startYear + 1})
      </span>
    </span>
  );
};

interface YearStatsProps {
  year: PlannerYearData;
}
const YearStats = ({ year }: YearStatsProps) => {
  let unitCount = 0;
  let courseCount = 0;
  year.quarters.forEach((quarter) => {
    quarter.courses.forEach((course) => {
      unitCount += course.minUnits;
      courseCount += 1;
    });
  });

  return (
    <p className="year-stats">
      <span className="course-count">{courseCount}</span> {pluralize(courseCount, 'courses', 'course')}
      {' • '}
      <span className="unit-count">{unitCount}</span> {pluralize(unitCount, 'units', 'unit')}
    </p>
  );
};

interface DeleteYearModalProps {
  show?: boolean;
  setShow: React.Dispatch<React.SetStateAction<boolean>>;
  yearName: string;
  yearIndex: number;
}

const DeleteYearModal = ({ show, setShow, yearName, yearIndex }: DeleteYearModalProps) => {
  const dispatch = useAppDispatch();
  const handleDeleteYear = () => {
    setShow(false);
    dispatch(deleteYear({ yearIndex }));
  };

  return (
    <Modal show={show} onHide={() => setShow(false)} centered className="ppc-modal">
      <Modal.Header closeButton>
        <h2>Delete Year</h2>
      </Modal.Header>
      <Modal.Body>
        <Box component="form" noValidate className="ppc-modal-form">
          <FormControl className="form-group">
            <p>Are you sure you want to delete {yearName || `Year ${yearIndex}`}?</p>
          </FormControl>
        </Box>

        <Button variant="danger" onClick={handleDeleteYear}>
          I am sure
        </Button>
        {/* <Form noValidate className="ppc-modal-form">
          <Form.Group className="form-group">
            <p>Are you sure you want to delete {yearName || `Year ${yearIndex}`}?</p>
          </Form.Group>
        </Form>
        <Button variant="danger" onClick={handleDeleteYear}>
          I am sure
        </Button> */}
      </Modal.Body>
    </Modal>
  );
};

interface YearProps {
  yearIndex: number;
  data: PlannerYearData;
}

const Year: FC<YearProps> = ({ yearIndex, data }) => {
  const dispatch = useAppDispatch();
  const [showContent, setShowContent] = useState(true);
  const [showEditYear, setShowEditYear] = useState(false);
  const [showDeleteYear, setShowDeleteYear] = useState(false);
  const [placeholderYear, setPlaceholderYear] = useState(data.startYear);
  const [placeholderName, setPlaceholderName] = useState(data.name);
  const yearContainerRef = useRef<HTMLDivElement>(null);

  const handleEditYearClick = () => {
    setPlaceholderYear(data.startYear);
    setPlaceholderName(data.name);
    setShowEditYear(true);
  };

  return (
    <Card className="year" ref={yearContainerRef} variant="outlined">
      <div className="year-header">
        <YearTitle year={data} index={yearIndex} />
        <YearStats year={data} />
        <div className="action-row">
          <IconButton onClick={handleEditYearClick}>
            <EditIcon />
          </IconButton>
          <IconButton onClick={() => setShowDeleteYear(true)}>
            <DeleteOutlineIcon />
          </IconButton>

          <ExpandMore
            expanded={showContent}
            onClick={() => setShowContent(!showContent)}
            aria-expanded={showContent}
            aria-label="expand planner"
          />
        </div>
      </div>
      <EditYearModal
        key={`edit-year-${placeholderYear}-${placeholderName}`}
        placeholderName={placeholderName ?? 'Year ' + (yearIndex + 1)}
        placeholderYear={placeholderYear}
        show={showEditYear}
        setShow={setShowEditYear}
        saveHandler={({ startYear, name, quarters }) => {
          setShowEditYear(false);
          if (startYear !== data.startYear) {
            setPlaceholderYear(startYear);
            dispatch(editYear({ startYear, index: yearIndex }));
          }
          if (name !== data.name) {
            setPlaceholderName(name);
            dispatch(editName({ name, index: yearIndex }));
          }
          const existing = data.quarters;
          let removed = 0;
          existing.forEach(({ name }, index) => {
            const remove = !quarters.find((q) => q.name === name);
            // Increment removed because the index of the quarters will change
            if (remove) dispatch(deleteQuarter({ yearIndex, quarterIndex: index - removed++ }));
          });
          const addQuarters = quarters.filter(({ name }) => !existing.find((q) => q.name === name));
          for (const { name } of addQuarters) {
            dispatch(addQuarter({ startYear, quarterData: { name, courses: [] } }));
          }
        }}
        currentQuarters={data.quarters.map((q) => q.name)}
        type="edit"
      />
      <DeleteYearModal show={showDeleteYear} setShow={setShowDeleteYear} yearName={data.name} yearIndex={yearIndex} />
      <Collapse in={showContent} timeout="auto" unmountOnExit>
        <Divider />
        <Card className="quarter-list" variant="outlined">
          {data.quarters.map((quarter, quarterIndex) => {
            return (
              <Quarter
                key={`year-quarter-${quarterIndex}`}
                yearIndex={yearIndex}
                quarterIndex={quarterIndex}
                data={quarter}
              />
            );
          })}
        </Card>
      </Collapse>
    </Card>
  );
};

export default Year;
