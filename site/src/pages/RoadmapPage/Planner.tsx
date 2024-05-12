import { FC, useEffect, useState } from 'react';
import { useCookies } from 'react-cookie';
import axios from 'axios';
import './Planner.scss';
import Header from './Header';
import AddYearPopup from './AddYearPopup';
import Year from './Year';
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import {
  selectYearPlans,
  setInvalidCourses,
  setTransfers,
  setUnsavedChanges,
  selectAllPlans,
  setAllPlans,
  defaultPlan,
} from '../../store/slices/roadmapSlice';
import { useFirstRender } from '../../hooks/firstRenderer';
import { SavedRoadmap, MongoRoadmap } from '../../types/types';
import { defaultYear, expandAllPlanners } from '../../helpers/planner';
import ImportTranscriptPopup from './ImportTranscriptPopup';
import { collapseAllPlanners, loadRoadmap, validatePlanner } from '../../helpers/planner';
import { Button, Modal } from 'react-bootstrap';

const Planner: FC = () => {
  const dispatch = useAppDispatch();
  const [cookies] = useCookies(['user']);
  const isFirstRenderer = useFirstRender();
  const currentPlanData = useAppSelector(selectYearPlans);
  const allPlanData = useAppSelector(selectAllPlans);
  const transfers = useAppSelector((state) => state.roadmap.transfers);
  const [showSyncModal, setShowSyncModal] = useState(false);

  const [missingPrerequisites, setMissingPrerequisites] = useState(new Set<string>());
  const roadmapStr = JSON.stringify({
    planners: collapseAllPlanners(allPlanData),
    transfers: transfers,
  });

  const handleLoadLocal = async () => {
    let roadmap: SavedRoadmap = null!;
    const roadmapItem = localStorage.getItem('roadmap');
    if (roadmapItem) {
      roadmap = JSON.parse(roadmapItem);
    }

    const planner = await expandAllPlanners(roadmap.planners);
    dispatch(setAllPlans(planner));
    dispatch(setTransfers(roadmap.transfers));
    setShowSyncModal(false);
  };

  const saveRoadmap = () => {
    const roadmap: SavedRoadmap = {
      timestamp: Date.now(),
      planners: collapseAllPlanners(allPlanData),
      transfers: transfers,
    };
    let savedAccount = false;
    // if logged in
    if (cookies.user !== undefined) {
      // save data to account
      const mongoRoadmap: MongoRoadmap = { _id: cookies.user.id, roadmap: roadmap };
      axios.post('/api/roadmap', mongoRoadmap);
      savedAccount = true;
    }

    // save to local storage as well
    localStorage.setItem('roadmap', JSON.stringify(roadmap));

    // mark changes as saved to bypass alert on page leave
    dispatch(setUnsavedChanges(false));

    if (savedAccount) {
      alert(`Roadmap saved under ${cookies.user.email}`);
    } else {
      alert('Roadmap saved locally! Login to save it to your account.');
    }
  };

  const calculatePlannerOverviewStats = () => {
    let unitCount = 0;
    let courseCount = 0;
    // sum up all courses
    currentPlanData.forEach((year) => {
      year.quarters.forEach((quarter) => {
        quarter.courses.forEach((course) => {
          unitCount += course.minUnits;
          courseCount += 1;
        });
      });
    });
    // add in transfer courses
    transfers.forEach((transfer) => {
      // only count if has both name and units
      if (transfer.units && transfer.name) {
        unitCount += transfer.units;
        courseCount += 1;
      }
    });
    return { unitCount, courseCount };
  };

  useEffect(() => {
    // stringify current roadmap

    // stringified value of an empty roadmap
    const emptyRoadmap = JSON.stringify({
      planners: [{ name: defaultPlan.name, content: [defaultYear()] }],
      transfers: [],
    } as Omit<SavedRoadmap, 'timestamp'>);

    // if first render and current roadmap is empty, load from local storage
    if (isFirstRenderer && roadmapStr === emptyRoadmap) {
      loadRoadmap(cookies, (planners, roadmap, isLocalNewer) => {
        dispatch(setAllPlans(planners));
        dispatch(setTransfers(roadmap.transfers));
        if (isLocalNewer) {
          setShowSyncModal(true);
        }
      });
    }
    // validate planner every time something changes
    else {
      validatePlanner(transfers, currentPlanData, (missing, invalid) => {
        // set missing courses
        setMissingPrerequisites(missing);
        // set the invalid courses
        dispatch(setInvalidCourses(invalid));
      });

      // check current roadmap against last-saved roadmap in local storage
      // if they are different, mark changes as unsaved to enable alert on page leave
      dispatch(setUnsavedChanges(localStorage.getItem('roadmap') !== roadmapStr));
    }
  }, [cookies, currentPlanData, dispatch, isFirstRenderer, roadmapStr, transfers]);

  const { unitCount, courseCount } = calculatePlannerOverviewStats();

  return (
    <div className="planner">
      <Header
        courseCount={courseCount}
        unitCount={unitCount}
        saveRoadmap={saveRoadmap}
        missingPrerequisites={missingPrerequisites}
      />
      <Modal
        show={showSyncModal}
        onHide={() => {
          setShowSyncModal(false);
        }}
      >
        <Modal.Header closeButton></Modal.Header>
        <Modal.Body>Detect local roadmap in the storage. Do you want to load local roadmap instead?</Modal.Body>
        <Modal.Footer>
          <Button variant="secondary" onClick={() => setShowSyncModal(false)}>
            No
          </Button>
          <Button variant="primary" onClick={handleLoadLocal}>
            Yes
          </Button>
        </Modal.Footer>
      </Modal>
      <Header
        courseCount={courseCount}
        unitCount={unitCount}
        saveRoadmap={saveRoadmap}
        missingPrerequisites={missingPrerequisites}
      />
      <section className="years">
        {currentPlanData.map((year, yearIndex) => {
          return <Year key={yearIndex} yearIndex={yearIndex} data={year} />;
        })}
      </section>
      <AddYearPopup
        placeholderName={'Year ' + (currentPlanData.length + 1)}
        placeholderYear={
          currentPlanData.length === 0
            ? new Date().getFullYear()
            : currentPlanData[currentPlanData.length - 1].startYear + 1
        }
      />
      <ImportTranscriptPopup />
    </div>
  );
};
export default Planner;
