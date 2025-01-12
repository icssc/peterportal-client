import { FC, useContext, useState } from 'react';
import './ImportZot4PlanPopup.scss';
import { FileEarmarkArrowDown } from 'react-bootstrap-icons';
import { Button, Form, Modal } from 'react-bootstrap';
//import { setTransfers, setYearPlans } from '../../store/slices/roadmapSlice';
//import { useAppDispatch } from '../../store/hooks';
import ThemeContext from '../../style/theme-context';
//import { BatchCourseData, PlannerQuarterData, PlannerYearData } from '../../types/types';
//import { quarters } from '@peterportal/types';
//import { searchAPIResults } from '../../helpers/util';
//import { QuarterName } from '@peterportal/types';
//import { normalizeQuarterName } from '../../helpers/planner';

const ImportZot4PlanPopup: FC = () => {
  const { darkMode } = useContext(ThemeContext);
  const [showModal, setShowModal] = useState(false);
  const [scheduleName, setScheduleName] = useState('');
  const [busy, setBusy] = useState(false);

  const importHandler = async () => {
    setBusy(true);
    try {
      // Use the backend route to try to obtain the formatted schedule
      //const { transfers, years } = await processTranscript(file);
      // Dispatch
      //dispatch(setTransfers(transfers));
      //dispatch(setYearPlans(Object.values(years)));
      // Hide modal?
      setShowModal(false);
    } finally {
      setBusy(false);
    }
  };

  return (
    <>
      <Modal show={showModal} onHide={() => setShowModal(false)} centered className="ppc-modal transcript-form">
        <Modal.Header closeButton>
          <h2>Import Schedule from Zot4Plan</h2>
        </Modal.Header>
        <Modal.Body>
          <Form className="ppc-modal-form">
            <Form.Group>
              <p>
                If you use{' '}
                <a target="_blank" href="https://zot4plan.com/" rel="noreferrer">
                  Zot4Plan
                </a>
                , you can add all your classes from that schedule to your PeterPortal roadmap. Your schedule in Zot4Plan
                will not be erased.
              </p>
              <p>Please enter the exact name that you use to save and load your Zot4Plan schedule.</p>
            </Form.Group>
            <Form.Group controlId="ScheduleName">
              <Form.Label className="ppc-modal-form-label">Schedule Name</Form.Label>
              <Form.Control
                type="text"
                placeholder="Exact Zot4Plan schedule name"
                onChange={(e) => setScheduleName(e.target.value)}
              />
              <span className="import-schedule-warn-text">
                {scheduleName.length > 0 && scheduleName.length < 8
                  ? 'Warning: no Zot4Plan schedule name contains less than 8 characters'
                  : ''}
              </span>
            </Form.Group>
          </Form>
          <Button variant="primary" disabled={busy} onClick={importHandler}>
            {busy ? 'Importing...' : 'Import'}
          </Button>
        </Modal.Body>
      </Modal>
      <Button
        variant={darkMode ? 'dark' : 'light'}
        className="ppc-btn import-schedule-btn"
        onClick={() => setShowModal(true)}
      >
        <FileEarmarkArrowDown className="import-schedule-icon" />
        <div>Import Zot4Plan Schedule</div>
      </Button>
    </>
  );
};

export default ImportZot4PlanPopup;
