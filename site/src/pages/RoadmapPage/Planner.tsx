import { FC, useEffect, useState } from 'react';
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
  setCourses,
} from '../../store/slices/roadmapSlice';
import { useFirstRender } from '../../hooks/firstRenderer';
import { SavedRoadmap } from '@peterportal/types';
import { convertLegacyLocalRoadmap, defaultYear, expandAllPlanners } from '../../helpers/planner';
import ImportTranscriptPopup from './ImportTranscriptPopup';
import { collapseAllPlanners, loadRoadmap, validatePlanner } from '../../helpers/planner';
import { Button, Modal } from 'react-bootstrap';
import trpc from '../../trpc';
import spawnToast from '../../helpers/toastify';
import { useIsLoggedIn } from '../../hooks/isLoggedIn';
import { closestCorners, DndContext } from '@dnd-kit/core';
import { arrayMove } from '@dnd-kit/sortable';

const Planner: FC = () => {
  const dispatch = useAppDispatch();
  const isLoggedIn = useIsLoggedIn();
  const isFirstRenderer = useFirstRender();
  const currentPlanData = useAppSelector(selectYearPlans);
  const allPlanData = useAppSelector(selectAllPlans);
  const transfers = useAppSelector((state) => state.roadmap.transfers);
  const [showSyncModal, setShowSyncModal] = useState(false);
  const [missingPrerequisites, setMissingPrerequisites] = useState(new Set<string>());
  const roadmapStr = JSON.stringify({
    planners: collapseAllPlanners(allPlanData).map((p) => ({ name: p.name, content: p.content })), // map to remove id attribute
    transfers: transfers,
  });
  // const dnd = useAppSelector((state) => state.dnd);

  const handleLoadLocal = async () => {
    let roadmap: SavedRoadmap = null!;
    const roadmapItem = localStorage.getItem('roadmap');
    if (roadmapItem) {
      roadmap = convertLegacyLocalRoadmap(JSON.parse(roadmapItem));
    }

    const planner = await expandAllPlanners(roadmap.planners);
    dispatch(setAllPlans(planner));
    dispatch(setTransfers(roadmap.transfers));
    setShowSyncModal(false);
  };

  const saveRoadmap = () => {
    const roadmap: SavedRoadmap = {
      timestamp: new Date().toISOString(),
      planners: collapseAllPlanners(allPlanData),
      transfers: transfers,
    };

    localStorage.setItem('roadmap', JSON.stringify(roadmap));

    // mark changes as saved to bypass alert on page leave
    dispatch(setUnsavedChanges(false));

    // if logged in, save data to account
    if (isLoggedIn) {
      trpc.roadmaps.save
        .mutate(roadmap)
        .then(() => {
          spawnToast(`Roadmap saved to your account!`);
        })
        .catch(() => {
          spawnToast('Roadmap saved locally! Login to save it to your account.');
        });
    } else {
      spawnToast('Roadmap saved locally! Login to save it to your account.');
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
      loadRoadmap(isLoggedIn, (planners, roadmap, isLocalNewer) => {
        dispatch(setAllPlans(planners));
        dispatch(setTransfers(roadmap.transfers));
        if (isLocalNewer) {
          setShowSyncModal(true);
        }
      });
    } else {
      validatePlanner(transfers, currentPlanData, (missing, invalid) => {
        // set missing courses
        setMissingPrerequisites(missing);
        // set the invalid courses
        dispatch(setInvalidCourses(invalid));
      });

      // check current roadmap against last-saved roadmap in local storage
      // if they are different, mark changes as unsaved to enable alert on page leave
      const localRoadmap: SavedRoadmap = JSON.parse(localStorage.getItem('roadmap') ?? emptyRoadmap);
      delete localRoadmap.timestamp;
      localRoadmap.planners = localRoadmap.planners.map((p) => ({ name: p.name, content: p.content })); // remove id attribute
      dispatch(setUnsavedChanges(JSON.stringify(localRoadmap) !== roadmapStr));
    }
  }, [isLoggedIn, currentPlanData, dispatch, isFirstRenderer, roadmapStr, transfers]);

  const { unitCount, courseCount } = calculatePlannerOverviewStats();

  return (
    <DndContext
      collisionDetection={closestCorners}
      onDragEnd={(e) => {
        // no destination
        if (!e.over) {
          return;
        }

        const overId = e.over.id.toString();
        const activeId = e.active.id.toString();

        const [yearIndex, quarterIndex, overCourseIndex] = overId
          .toString()
          .split('-')
          .map((x) => Number(x));
        const [, , activeCourseIndex] = activeId
          .toString()
          .split('-')
          .map((x) => Number(x));
        const courses = currentPlanData[Number(yearIndex)].quarters[Number(quarterIndex)].courses;

        dispatch(
          setCourses({ yearIndex, quarterIndex, courses: arrayMove(courses, activeCourseIndex, overCourseIndex) }),
        );

        //move from planner to coursebag
        // if (result.destination.droppableId === 'coursebag' && result.source.droppableId != 'coursebag') {
        //   const [yearIndex, quarterIndex]: string[] = result.source.droppableId.split('-');
        //   const course = roadmap[parseInt(yearIndex)].quarters[parseInt(quarterIndex)].courses[result.source.index];
        //   addCourseToBag(course);
        //   dispatch(
        //     deleteCourse({
        //       yearIndex: parseInt(yearIndex),
        //       quarterIndex: parseInt(quarterIndex),
        //       courseIndex: result.source.index,
        //     }),
        //   );

        //   return;
        // }

        // if (result.source.droppableId === 'coursebag' && result.destination.droppableId != 'coursebag') {
        //   const course = coursebag[result.source.index];

        //   removeCourseFromBag(course);
        // }

        // if source == search and destination != search
      }}
      onDragOver={(e) => {
        console.log('over', e.over);
        // if (!e.over) {
        //   return;
        // }
        // const overId = e.over.id.toString().split('|')[0];
        // if (e.over && fromContainer && overId !== fromContainer) {
        //   dispatch(
        //     moveToContainer({
        //       oldContainerId: fromContainer.toString(),
        //       newContainerId: overId,
        //       itemId: e.active.id.toString(),
        //     }),
        //   );
        // }
      }}
    >
      <div className="planner">
        <Modal
          show={showSyncModal}
          onHide={() => {
            setShowSyncModal(false);
          }}
          className="ppc-modal"
          centered
        >
          <Modal.Header closeButton>
            <h2>Roadmap Out of Sync</h2>
          </Modal.Header>
          <Modal.Body>
            <p>
              This device's saved roadmap has newer changes than the one saved to your account. Where would you like to
              load your roadmap from?
            </p>
          </Modal.Body>
          <Modal.Footer>
            <Button variant="primary" onClick={handleLoadLocal}>
              This Device
            </Button>
            <Button variant="secondary" onClick={() => setShowSyncModal(false)}>
              My Account
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
    </DndContext>
  );
};
export default Planner;
