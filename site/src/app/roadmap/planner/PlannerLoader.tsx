'use client';
import { FC, useCallback, useEffect, useState } from 'react';
import { Modal } from 'react-bootstrap';
import { Button, Stack } from '@mui/material';
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
  selectYearPlans,
  setInitialPlannerData,
  setInvalidCourses,
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
import { compareRoadmaps, restoreRevision } from '../../../helpers/roadmap';
import { deepCopy } from '../../../helpers/util';

function useCheckUnsavedChanges() {
  const currentIndex = useAppSelector((state) => state.roadmap.currentRevisionIndex);
  const lastSavedIndex = useAppSelector((state) => state.roadmap.savedRevisionIndex);

  const planners = useAppSelector((state) => state.roadmap.plans);
  const revisions = useAppSelector((state) => state.roadmap.revisions);
  const currIdx = useAppSelector((state) => state.roadmap.currentRevisionIndex);
  const lastSaveIdx = useAppSelector((state) => state.roadmap.savedRevisionIndex);

  useEffect(() => {
    if (currentIndex === lastSavedIndex) return;
    const alertIfUnsaved = (event: BeforeUnloadEvent) => {
      const lastSavedRoadmapPlans = deepCopy(planners);
      restoreRevision(lastSavedRoadmapPlans, revisions, currIdx, lastSaveIdx);
      const collapsedPrevious = collapseAllPlanners(lastSavedRoadmapPlans);
      const collapsedCurrent = collapseAllPlanners(planners);
      const diffs = compareRoadmaps(collapsedPrevious, collapsedCurrent);

      const isDifferent = Object.values(diffs).some((val) => Array.isArray(val) && val.length > 0);
      if (isDifferent) event.preventDefault();
    };
    window.addEventListener('beforeunload', alertIfUnsaved);
    return () => window.removeEventListener('beforeunload', alertIfUnsaved);
  });
}

const PlannerLoader: FC = () => {
  const [showSyncModal, setShowSyncModal] = useState(false);
  const userTransfersLoaded = useAppSelector((state) => state.transferCredits.dataLoadState === 'done');
  const transferred = useTransferredCredits();
  const currentPlanData = useAppSelector(selectYearPlans);
  const isRoadmapLoading = useAppSelector((state) => state.roadmap.roadmapLoading);
  const isLoggedIn = useIsLoggedIn();
  useCheckUnsavedChanges();

  const [roadmapLoaded, setRoadmapLoaded] = useState(false);
  const [initialLocalRoadmap, setInitialLocalRoadmap] = useState<SavedRoadmap | null>(null);
  const [initialAccountRoadmap, setInitialAccountRoadmap] = useState<SavedRoadmap | null>(null);

  const dispatch = useAppDispatch();

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
      const plans = await expandAllPlanners(roadmap.planners);
      const timestamp = new Date(roadmap.timestamp ?? Date.now()).getTime();
      dispatch(setInitialPlannerData({ plans, timestamp }));
      dispatch(setRoadmapLoading(false));
    },
    [dispatch],
  );

  // save function will update localStorage (thus comparisons above will work) and account roadmap
  const saveRoadmapAndUpsertTransfers = useCallback(
    async (collapsedPlans: SavedPlannerData[]) => {
      // Cannot be called before format is upgraded from single to multi-planner
      await saveRoadmap(isLoggedIn, null, collapsedPlans, false);

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
    if (!userTransfersLoaded || !initialLocalRoadmap || roadmapLoaded) return;

    populateExistingRoadmap(initialAccountRoadmap ?? initialLocalRoadmap).then(() => {
      setRoadmapLoaded(true);

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
    roadmapLoaded,
  ]);

  // Validate Courses on change
  useEffect(() => {
    const transferNames = getNamesOfTransfers(transferred.courses, transferred.ap, transferred.apInfo);
    const { invalidCourses } = validatePlanner(transferNames, currentPlanData);
    dispatch(setInvalidCourses(invalidCourses));
  }, [dispatch, currentPlanData, transferred]);

  const overrideAccountRoadmap = async () => {
    const localRoadmap = readLocalRoadmap<SavedRoadmap>();

    // Update the account roadmap using local data
    await saveRoadmapAndUpsertTransfers(localRoadmap.planners);
    const roadmapWithIds = await loadRoadmap(true).then((res) => res.accountRoadmap!);

    // Update frontend state to show local data
    populateExistingRoadmap(roadmapWithIds);
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
        <Stack direction="row" spacing={2}>
          {/* When the Modal is migrated to MUI, should remove the Stack used for spacing here */}
          <Button onClick={overrideAccountRoadmap}>This Device</Button>
          <Button color="inherit" onClick={() => setShowSyncModal(false)}>
            My Account
          </Button>
        </Stack>
      </Modal.Footer>
    </Modal>
  );
};

export default PlannerLoader;
