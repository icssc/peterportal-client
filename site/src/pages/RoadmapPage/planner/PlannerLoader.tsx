import { FC, useCallback, useEffect, useState } from 'react';
import { Button, Modal } from 'react-bootstrap';

import trpc from '../../../trpc';
import { SavedPlannerData, SavedRoadmap } from '@peterportal/types';
import {
  selectAllPlans,
  selectYearPlans,
  setAllPlans,
  setInvalidCourses,
  setUnsavedChanges,
  setRoadmapLoading,
} from '../../../store/slices/roadmapSlice';
import { setDataLoadState } from '../../../store/slices/transferCreditsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { useTransferredCredits } from '../../../hooks/transferCredits';
import { useIsLoggedIn } from '../../../hooks/isLoggedIn';
import {
  getNamesOfTransfers,
  loadTransferredAPs,
  loadTransferredCourses,
  loadTransferredGEs,
  loadTransferredOther,
} from '../../../helpers/transferCredits';
import {
  collapseAllPlanners,
  expandAllPlanners,
  loadRoadmap,
  saveRoadmap,
  upgradeLocalRoadmap,
  validatePlanner,
} from '../../../helpers/planner';

const PlannerLoader: FC = () => {
  const [showSyncModal, setShowSyncModal] = useState(false);
  const [formatUpgraded, setFormatUpgraded] = useState(false);
  const userTransfersLoaded = useAppSelector((state) => state.transferCredits.dataLoadState === 'done');
  const transferred = useTransferredCredits();
  const allPlanData = useAppSelector(selectAllPlans);
  const currentPlanData = useAppSelector(selectYearPlans);
  const isLoggedIn = useIsLoggedIn();

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

  // hook to upgrade BEFORE loading the roadmap
  useEffect(() => {
    if (formatUpgraded) return;

    upgradeLocalRoadmap().then(async (roadmap: SavedRoadmap) => {
      // has legacy transfers => operation was not completed (because another upgrade is in progress)
      if (roadmap.transfers.length) return;
      setFormatUpgraded(true);
      dispatch(setDataLoadState('loading'));
    });
  }, [dispatch, isLoggedIn, formatUpgraded]);

  useEffect(() => {
    dispatch(setRoadmapLoading(true));
  }, [dispatch]);

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
    // don't load if format is not up to date or transfers are not loaded
    if (!formatUpgraded || !userTransfersLoaded) return;

    loadRoadmap(isLoggedIn).then(async ({ accountRoadmap, localRoadmap }) => {
      await populateExistingRoadmap(accountRoadmap ?? localRoadmap);
      if (!isLoggedIn) return;
      const isLocalNewer = new Date(localRoadmap.timestamp ?? 0) > new Date(accountRoadmap?.timestamp ?? 0);

      // ignore local changes if account is newer
      if (!isLocalNewer) return;

      // Logged in + roadmap does exist but is older => prompt
      if (accountRoadmap) return setShowSyncModal(true);

      // Logged in + doesn't exist => update everything
      saveRoadmapAndUpsertTransfers(localRoadmap.planners);
    });
  }, [formatUpgraded, saveRoadmapAndUpsertTransfers, isLoggedIn, populateExistingRoadmap, userTransfersLoaded]);

  // Validate Courses on change
  useEffect(() => {
    const transferNames = getNamesOfTransfers(transferred.courses, transferred.ap, transferred.apInfo);
    const { invalidCourses } = validatePlanner(transferNames, currentPlanData);
    dispatch(setInvalidCourses(invalidCourses));
  }, [dispatch, currentPlanData, transferred]);

  // check roadmapStr (current planner) against localStorage planner
  useEffect(() => {
    loadRoadmap(false).then(({ localRoadmap }) => {
      // Remove timestamp and Plan IDs when comparing content
      delete localRoadmap.timestamp;
      localRoadmap.planners = localRoadmap.planners.map((p) => ({ name: p.name, content: p.content }));
      dispatch(setUnsavedChanges(JSON.stringify(localRoadmap) !== roadmapStr));
    });
  }, [dispatch, roadmapStr]);

  const overrideAccountRoadmap = async () => {
    const { localRoadmap } = await loadRoadmap(false);

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
