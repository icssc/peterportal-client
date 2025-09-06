'use client';
import { FC, useCallback, useEffect, useState } from 'react';
import { Button, Modal } from 'react-bootstrap';
import {
  collapseAllPlanners,
  expandAllPlanners,
  loadRoadmap,
  readLocalRoadmap,
  saveRoadmap,
  validatePlanner,
} from '../../../helpers/planner';
import { SavedPlannerData, SavedRoadmap } from '@peterportal/types';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import {
  selectAllPlans,
  selectYearPlans,
  setAllPlans,
  setInvalidCourses,
  setUnsavedChanges,
  setRoadmapLoading,
} from '../../../store/slices/roadmapSlice';
import { useIsLoggedIn } from '../../../hooks/isLoggedIn';
import {
  getNamesOfTransfers,
  loadTransferredAPs,
  loadTransferredCourses,
  loadTransferredGEs,
  loadTransferredOther,
} from '../../../helpers/transferCredits';
import { useTransferredCredits } from '../../../hooks/transferCredits';
import trpc from '../../../trpc';
import { setDataLoadState } from '../../../store/slices/transferCreditsSlice';

const PlannerLoader: FC = () => {
  const [showSyncModal, setShowSyncModal] = useState(false);
  const userTransfersLoaded = useAppSelector((state) => state.transferCredits.dataLoadState === 'done');
  const transferred = useTransferredCredits();
  const allPlanData = useAppSelector(selectAllPlans);
  const currentPlanData = useAppSelector(selectYearPlans);
  const isRoadmapLoading = useAppSelector((state) => state.roadmap.roadmapLoading);
  const isLoggedIn = useIsLoggedIn();

  const [initialLocalRoadmap, setInitialLocalRoadmap] = useState<SavedRoadmap | null>(null);
  const [initialAccountRoadmap, setInitialAccountRoadmap] = useState<SavedRoadmap | null>(null);

  const dispatch = useAppDispatch();

  const roadmapStr = JSON.stringify({
    planners: collapseAllPlanners(allPlanData).map((p) => ({ name: p.name, content: p.content })), // map to remove id attribute
    transfers: [], // should be empty anyways once upgrade runs
  });

  const loadLocalTransfers = async () => {
    const courses = await loadTransferredCourses(false);
    const ap = await loadTransferredAPs(false);
    const ge = await loadTransferredGEs(false);
    const other = await loadTransferredOther(false);
    return { courses, ap, ge, other };
  };

  // Defaults to account if it exists because local can override it in a different helper
  const populateExistingRoadmap = useCallback(
    async (roadmap: SavedRoadmap) => {
      const planners = await expandAllPlanners(roadmap.planners);
      dispatch(setAllPlans(planners));
      dispatch(setRoadmapLoading(false));
    },
    [dispatch],
  );

  // save function will update localStorage (thus comparisons above will work) and account roadmap
  const saveRoadmapAndUpsertTransfers = useCallback(
    async (collapsedPlans: SavedPlannerData[]) => {
      // Cannot be called before format is upgraded
      await saveRoadmap(isLoggedIn, collapsedPlans, false);

      // mark changes as saved to bypass alert on page leave
      dispatch(setUnsavedChanges(false));

      // upsert transfers
      const { courses, ap, ge, other } = await loadLocalTransfers();
      await trpc.transferCredits.overrideAllTransfers.mutate({ courses, ap, ge, other });

      // the user data in redux is not correct and is thus "not loaded yet"
      // force-setting this is ok because it's equivalent to not having data loaded yet
      dispatch(setDataLoadState('loading'));
    },
    [dispatch, isLoggedIn],
  );

  useEffect(() => {
    dispatch(setRoadmapLoading(true));
  }, [dispatch]);

  // Read & upgrade the local roadmap, then trigger loading transfers
  useEffect(() => {
    // must wait for setRoadmapLoading(true) since that change will only trigger this useEffect once
    // This is to avoid issues with loadRoadmap() being called twice.
    if (!isRoadmapLoading || initialAccountRoadmap || initialLocalRoadmap) return;

    loadRoadmap(isLoggedIn).then(({ accountRoadmap, localRoadmap }) => {
      setInitialAccountRoadmap(accountRoadmap);
      setInitialLocalRoadmap(localRoadmap);
      dispatch(setDataLoadState('loading'));
    });
  }, [dispatch, isRoadmapLoading, initialAccountRoadmap, initialLocalRoadmap, isLoggedIn]);

  // After transfers loaded, do roadmap conflict resolution
  useEffect(() => {
    if (!userTransfersLoaded || !initialLocalRoadmap) return;

    populateExistingRoadmap(initialAccountRoadmap ?? initialLocalRoadmap).then(() => {
      if (!isLoggedIn) return;
      const isLocalNewer =
        new Date(initialLocalRoadmap.timestamp ?? 0) > new Date(initialAccountRoadmap?.timestamp ?? 0);

      // ignore local changes if account is newer
      if (!isLocalNewer) return;

      // Logged in + roadmap does exist but is older => prompt
      if (initialAccountRoadmap) return setShowSyncModal(true);

      // Logged in + doesn't exist => update everything
      saveRoadmapAndUpsertTransfers(initialLocalRoadmap.planners);
    });
  }, [
    saveRoadmapAndUpsertTransfers,
    isLoggedIn,
    populateExistingRoadmap,
    userTransfersLoaded,
    initialAccountRoadmap,
    initialLocalRoadmap,
  ]);

  // Validate Courses on change
  useEffect(() => {
    const transferNames = getNamesOfTransfers(transferred.courses, transferred.ap, transferred.apInfo);
    const { invalidCourses } = validatePlanner(transferNames, currentPlanData);
    dispatch(setInvalidCourses(invalidCourses));
  }, [dispatch, currentPlanData, transferred]);

  // check roadmapStr (current planner) against localStorage planner
  useEffect(() => {
    if (!initialAccountRoadmap) return; // can't reliably read until initial roadmap loads

    const localRoadmap = readLocalRoadmap<SavedRoadmap>();
    // Remove timestamp and Plan IDs when comparing content
    delete localRoadmap.timestamp;
    localRoadmap.planners = localRoadmap.planners.map((p) => ({ name: p.name, content: p.content }));
    dispatch(setUnsavedChanges(JSON.stringify(localRoadmap) !== roadmapStr));
  }, [dispatch, roadmapStr, initialAccountRoadmap]);

  const overrideAccountRoadmap = async () => {
    const localRoadmap = readLocalRoadmap<SavedRoadmap>();

    // Update the account roadmap using local data
    await saveRoadmapAndUpsertTransfers(localRoadmap.planners);

    // Update frontend state to show local data
    const planner = await expandAllPlanners(localRoadmap.planners);
    dispatch(setAllPlans(planner));
    setShowSyncModal(false);
  };

  return (
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
          This device's saved roadmap has newer changes than the one saved to your account. Where would you like to load
          your roadmap from?
        </p>
      </Modal.Body>
      <Modal.Footer>
        <Button variant="primary" onClick={overrideAccountRoadmap}>
          This Device
        </Button>
        <Button variant="secondary" onClick={() => setShowSyncModal(false)}>
          My Account
        </Button>
      </Modal.Footer>
    </Modal>
  );
};

export default PlannerLoader;
