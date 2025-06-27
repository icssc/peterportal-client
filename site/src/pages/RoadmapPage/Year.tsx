import { FC, useRef, useState } from 'react';
import './Year.scss';
import { Button, Modal, Form } from 'react-bootstrap';
import Quarter from './Quarter';
import { useAppDispatch } from '../../store/hooks';
import { addQuarter, editYear, editName, deleteYear, deleteQuarter } from '../../store/slices/roadmapSlice';
import { pluralize } from '../../helpers/util';

import { PlannerYearData } from '../../types/types';
import EditYearModal from './YearModal';

import { Card, Collapse, Divider, IconButton } from '@mui/material';
import EditIcon from '@mui/icons-material/Edit';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import { ExpandMore } from '../../component/ExpandMore/ExpandMore';
import { CSSTransition } from 'react-transition-group';

// TODO: everywhere possible (search for ' • ' or ', '), combine courses and units into helper function

interface YearStatsProps {
  year: PlannerYearData;
}
const YearStats = ({ year }: YearStatsProps) => {
  const courses = year.quarters.flatMap((q) => q.courses);
  const unitCount = courses.reduce((sum, c) => sum + c.minUnits, 0);
  const courseCount = courses.length;

  return (
    <p className="year-stats">
      <b>{courseCount}</b> course{pluralize(courseCount)}
      {' • '}
      <b>{unitCount}</b> unit{pluralize(unitCount)}
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
    <CSSTransition in={show} timeout={500} unmountOnExit>
      <Modal show={show} onHide={() => setShow(false)} centered className="ppc-modal">
        <Modal.Header closeButton>
          <h2>Delete Year</h2>
        </Modal.Header>
        <Modal.Body>
          <Form noValidate className="ppc-modal-form">
            <Form.Group>
              <p>Are you sure you want to delete {yearName || `Year ${yearIndex}`}?</p>
            </Form.Group>
          </Form>
          <Button variant="danger" onClick={handleDeleteYear}>
            I am sure
          </Button>
        </Modal.Body>
      </Modal>
    </CSSTransition>
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
        <span className="year-title">
          <b>{data.name ?? `Year ${yearIndex + 1}`}</b>{' '}
          <span className="year-range">({`${data.startYear}-${data.startYear + 1}`})</span>
        </span>
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
