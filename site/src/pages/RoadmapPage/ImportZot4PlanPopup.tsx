import { FC, useContext, useState } from 'react';
import './ImportZot4PlanPopup.scss';
import { FileEarmarkArrowDown } from 'react-bootstrap-icons';
import { Button, Form, Modal } from 'react-bootstrap';
import { addRoadmapPlan, setPlanIndex, selectAllPlans } from '../../store/slices/roadmapSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import ThemeContext from '../../style/theme-context';
import trpc from '../../trpc.ts';
import { expandAllPlanners } from '../../helpers/planner';
import spawnToast from '../../helpers/toastify';
import helpImage from '../../asset/zot4plan-import-help.png';

const ImportZot4PlanPopup: FC = () => {
  const dispatch = useAppDispatch();
  const { darkMode } = useContext(ThemeContext);
  const [showModal, setShowModal] = useState(false);
  const [scheduleName, setScheduleName] = useState('');
  const [busy, setBusy] = useState(false);
  const allPlanData = useAppSelector(selectAllPlans);

  const obtainImportedRoadmap = async (query: string) => {
    // Get the result
    const result = await trpc.zot4PlanImportRouter.getScheduleFormatted.query({
      scheduleName: query,
    });
    // Verify that the result has one planner (if not, the import failed)
    if (result.planners.length == 0) {
      // Notify the user
      // TODO: improve the toast notification?
      spawnToast('The schedule named "' + query + '" could not be successfully imported', true);
      return;
    }
    // Expand the result
    const expandedPlanners = await expandAllPlanners(result.planners);
    // Add the expanded result as a new planner to the roadmap
    // TODO: handling invalid course names, reporting errors (in case there was a formatting issue)
    const currentPlanDataLength = allPlanData.length;
    dispatch(addRoadmapPlan(expandedPlanners[0]));
    dispatch(setPlanIndex(currentPlanDataLength));
    // TODO: ensure the page scrolls up to the top
    // TODO: fix bug in dropdown
  };

  const handleImport = async () => {
    setBusy(true);
    try {
      // Use the backend route to try to obtain the formatted schedule
      await obtainImportedRoadmap(scheduleName);
      // Success; hide the modal
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
                , you can add all your classes from that schedule to a new roadmap in PeterPortal. Your schedule in
                Zot4Plan and your current roadmaps will not be modified.
              </p>
              <p>Please enter the exact name that you use to save and load your Zot4Plan schedule, as shown here:</p>
              <img
                className="w-100"
                src={helpImage}
                alt="Screenshot of Zot4Plan's save feature where the schedule name is typically used"
              />
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
          <Button variant="primary" disabled={busy || scheduleName.length < 8} onClick={handleImport}>
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
