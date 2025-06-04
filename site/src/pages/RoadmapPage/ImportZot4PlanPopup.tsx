import { FC, useContext, useState } from 'react';
import './ImportZot4PlanPopup.scss';
import { Button, Form, Modal } from 'react-bootstrap';
import { setPlanIndex, selectAllPlans, addRoadmapPlan } from '../../store/slices/roadmapSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import ThemeContext from '../../style/theme-context';
import trpc from '../../trpc.ts';
import { collapseAllPlanners, expandAllPlanners, makeUniquePlanName, saveRoadmap } from '../../helpers/planner';
import spawnToast from '../../helpers/toastify';
import helpImage from '../../asset/zot4plan-import-help.png';
import { useIsLoggedIn } from '../../hooks/isLoggedIn.ts';
import { useTransferredCredits } from '../../hooks/transferCredits';
import { setUserAPExams } from '../../store/slices/transferCreditsSlice';

import CloudDownloadIcon from '@mui/icons-material/CloudDownload';
import WarningAmberIcon from '@mui/icons-material/WarningAmber';

const ImportZot4PlanPopup: FC = () => {
  const dispatch = useAppDispatch();
  const { darkMode } = useContext(ThemeContext);
  const isLoggedIn = useIsLoggedIn();
  const [showModal, setShowModal] = useState(false);
  const [scheduleName, setScheduleName] = useState('');
  const [studentYear, setStudentYear] = useState('1');
  const [busy, setBusy] = useState(false);
  const allPlanData = useAppSelector(selectAllPlans);
  const apExams = useTransferredCredits().ap;

  const obtainImportedRoadmap = async (schedName: string, currYear: string) => {
    // Get the result
    try {
      const { savedRoadmap, apExams: z4pApExams } = await trpc.zot4PlanImport.getScheduleFormatted.query({
        scheduleName: schedName,
        studentYear: currYear,
      });

      // Combine added AP exams with AP exams from Zot4Plan; ignore any exams that were already added
      const newExams = z4pApExams.filter(
        (imported) => !apExams.some((existing) => existing.examName === imported.examName),
      );

      const combinedExams = apExams.concat(newExams);
      dispatch(setUserAPExams(combinedExams));

      // Add new AP exam rows
      if (isLoggedIn) {
        await trpc.transferCredits.overrideAllTransfers.mutate({
          courses: [],
          ap: combinedExams,
          ge: [],
          other: [],
        });
      }

      // Expand the result
      const expandedPlanners = await expandAllPlanners(savedRoadmap.planners);
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

      const collapsed = collapseAllPlanners(updatedPlans);
      await saveRoadmap(isLoggedIn, collapsed, true);
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
      <Modal show={showModal} onHide={() => setShowModal(false)} centered className="ppc-modal multiplan-modal">
        <Modal.Header closeButton>
          <h2>Import Schedule from Zot4Plan</h2>
        </Modal.Header>
        <Modal.Body>
          <Form className="ppc-modal-form">
            <Form.Group>
              <p>
                To add your{' '}
                <a target="_blank" href="https://zot4plan.com/" rel="noreferrer">
                  Zot4Plan
                </a>{' '}
                classes into a new roadmap, enter the exact name that you used to save your Zot4Plan schedule (as shown
                below).
              </p>
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
                  <WarningAmberIcon className="import-schedule-icon" />
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
      <Button variant={darkMode ? 'dark' : 'light'} className="ppc-btn" onClick={() => setShowModal(true)}>
        <CloudDownloadIcon />
        <span>Zot4Plan Schedule</span>
      </Button>
    </>
  );
};

export default ImportZot4PlanPopup;
