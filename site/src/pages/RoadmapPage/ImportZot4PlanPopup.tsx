import { FC, useContext, useState } from 'react';
import './ImportZot4PlanPopup.scss';
import { Button, Form, Modal } from 'react-bootstrap';
import { setPlanIndex, selectAllPlans, RoadmapPlan, addRoadmapPlan } from '../../store/slices/roadmapSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import ThemeContext from '../../style/theme-context';
import trpc from '../../trpc.ts';
import { expandAllPlanners, makeUniquePlanName } from '../../helpers/planner';
import spawnToast from '../../helpers/toastify';
import helpImage from '../../asset/zot4plan-import-help.png';

import CloudDownloadIcon from '@mui/icons-material/CloudDownload';
import WarningAmberIcon from '@mui/icons-material/WarningAmber';

interface ImportZot4PlanPopupProps {
  saveRoadmap: (planner?: RoadmapPlan[]) => Promise<void>;
}
const ImportZot4PlanPopup: FC<ImportZot4PlanPopupProps> = ({ saveRoadmap }) => {
  const dispatch = useAppDispatch();
  const { darkMode } = useContext(ThemeContext);
  const [showModal, setShowModal] = useState(false);
  const [scheduleName, setScheduleName] = useState('');
  const [studentYear, setStudentYear] = useState('1');
  const [busy, setBusy] = useState(false);
  const allPlanData = useAppSelector(selectAllPlans);

  const obtainImportedRoadmap = async (schedName: string, currYear: string) => {
    // Get the result
    try {
      const result = await trpc.zot4PlanImportRouter.getScheduleFormatted.query({
        scheduleName: schedName,
        studentYear: currYear,
      });
      // Expand the result
      const expandedPlanners = await expandAllPlanners(result.planners);
      // Check for validity: length and invalid course names
      if (expandedPlanners.length < 1) {
        spawnToast('The schedule "' + schedName + '" could not be imported', true);
        return;
      }
      // Unknown (undefined) course names will crash PeterPortal if loaded, so remove them
      let problemCount = 0;
      for (const yearPlan of expandedPlanners[0].content.yearPlans) {
        for (const quarter of yearPlan.quarters) {
          const newCourses = quarter.courses.filter((course) => course != undefined);
          problemCount += quarter.courses.length - newCourses.length;
          quarter.courses = newCourses;
        }
      }
      if (problemCount > 0) {
        spawnToast('Partially imported "' + schedName + '" (removed ' + problemCount + ' unknown course(s)', true);
      }
      expandedPlanners[0].name = makeUniquePlanName(expandedPlanners[0].name, allPlanData);
      const updatedPlans = [...allPlanData, expandedPlanners[0]];
      dispatch(addRoadmapPlan(expandedPlanners[0]));
      dispatch(setPlanIndex(updatedPlans.length - 1));
      await saveRoadmap(updatedPlans);
    } catch (err) {
      // Notify the user
      spawnToast('The schedule "' + schedName + '" could not be retrieved', true);
    }
  };

  const handleImport = async () => {
    setBusy(true);
    try {
      // Use the backend route to try to obtain the formatted schedule
      await obtainImportedRoadmap(scheduleName, studentYear);
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
                onKeyDown={(e: React.KeyboardEvent) => {
                  // prevent submitting form (reloads the page)
                  if (e.key === 'Enter') {
                    e.preventDefault();
                  }
                }}
              />
              {scheduleName.length > 0 && scheduleName.length < 8 && (
                <span className="import-schedule-warning">
                  <WarningAmberIcon className="import-schedule-warning-icon" />
                  No Zot4Plan schedule name contains less than 8 characters
                </span>
              )}
            </Form.Group>
            <Form.Group controlId="CurrentYear">
              <Form.Label className="ppc-modal-form-label">I am currently a...</Form.Label>
              <Form.Control as="select" onChange={(ev) => setStudentYear(ev.target.value)} value={studentYear}>
                <option value="1" selected>
                  1st year
                </option>
                <option value="2">2nd year</option>
                <option value="3">3rd year</option>
                <option value="4">4th year</option>
              </Form.Control>
            </Form.Group>
          </Form>
          <Button variant="primary" disabled={busy || scheduleName.length < 8} onClick={handleImport}>
            {busy ? 'Importing...' : 'Import and Save'}
          </Button>
        </Modal.Body>
      </Modal>
      <Button
        variant={darkMode ? 'dark' : 'light'}
        className="ppc-btn import-schedule-btn"
        onClick={() => setShowModal(true)}
      >
        <CloudDownloadIcon className="import-schedule-icon" />
        <div>Import Zot4Plan Schedule</div>
      </Button>
    </>
  );
};

export default ImportZot4PlanPopup;
