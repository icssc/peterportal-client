'use client';
import { FC, useCallback, useEffect, useState } from 'react';
import { Button, Dialog, DialogContent, DialogContentText, DialogTitle, Divider } from '@mui/material';
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
  setToastMsg,
  setToastSeverity,
  setShowToast,
  updateRoadmapCustomCourse,
  updateTempPlannerIds,
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
import { setCustomCourses } from '../../../store/slices/customCourseSlice';

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
      dispatch(setInitialPlannerData({ plans, timestamp, currentPlanIndex: roadmap.currentPlanIndex ?? 0 }));
      dispatch(setRoadmapLoading(false));
    },
    [dispatch],
  );

  // save function will update localStorage (thus comparisons above will work) and account roadmap
  const saveRoadmapAndUpsertTransfers = useCallback(
    async (
      collapsedLocalPlans: SavedPlannerData[],
      collapsedAccountPlans: SavedPlannerData[] | null,
      currentPlanIndex?: number,
    ) => {
      // Cannot be called before format is upgraded from single to multi-planner
      const result = await saveRoadmap(isLoggedIn, collapsedAccountPlans, collapsedLocalPlans, currentPlanIndex);

      if (result.success && isLoggedIn) {
        dispatch(setToastMsg('Roadmap saved to your account!'));
        dispatch(setToastSeverity('success'));
        dispatch(setShowToast(true));
      } else if (result.success && !isLoggedIn) {
        setToastMsg('Roadmap saved locally! Log in to save it to your account');
        dispatch(setToastSeverity('success'));
        dispatch(setShowToast(true));
      } else if (!result.success) {
        setToastMsg('Unable to save roadmap to your account');
        setToastSeverity('error');
        setShowToast(true);
      }

      if (result.success && result.plannerIdLookup) {
        dispatch(updateTempPlannerIds(result.plannerIdLookup));
      }

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

      trpc.customCourses.getCustomCards.query().then((cards) => {
        const customCourses = cards.map((c) => ({
          id: c.id,
          courseName: c.name,
          units: c.units,
          description: c.description,
        }));
        dispatch(setCustomCourses(customCourses));
        customCourses.forEach((course) => dispatch(updateRoadmapCustomCourse(course)));
      });

      const isLocalNewer =
        new Date(initialLocalRoadmap.timestamp ?? 0) > new Date(initialAccountRoadmap?.timestamp ?? 0);

      // ignore local changes if account is newer
      if (!isLocalNewer) return;

      // Logged in + roadmap does exist but is older => prompt
      if (initialAccountRoadmap) return setShowSyncModal(true);

      // Logged in + doesn't exist => update everything
      saveRoadmapAndUpsertTransfers(initialLocalRoadmap.planners, null, initialLocalRoadmap.currentPlanIndex);
    });
  }, [
    saveRoadmapAndUpsertTransfers,
    isLoggedIn,
    populateExistingRoadmap,
    userTransfersLoaded,
    initialAccountRoadmap,
    initialLocalRoadmap,
    roadmapLoaded,
    dispatch,
  ]);

  // Validate Courses on change
  useEffect(() => {
    const transferNames = getNamesOfTransfers(transferred.courses, transferred.ap, transferred.apInfo);
    const { invalidCourses } = validatePlanner(transferNames, currentPlanData);
    dispatch(setInvalidCourses(invalidCourses));
  }, [dispatch, currentPlanData, transferred]);

  const [overrideLoading, setOverrideLoading] = useState(false);

  const overrideAccountRoadmap = async () => {
    setOverrideLoading(true);
    const localRoadmap = readLocalRoadmap<SavedRoadmap>();

    // Update the account roadmap using local data
    await saveRoadmapAndUpsertTransfers(
      localRoadmap.planners,
      initialAccountRoadmap?.planners ?? null,
      localRoadmap.currentPlanIndex,
    );
    const roadmapWithIds = await loadRoadmap(true).then((res) => res.accountRoadmap!);

    // Update frontend state to show local data
    populateExistingRoadmap(roadmapWithIds);
    setShowSyncModal(false);
  };

  const [accountLoading, setAccountLoading] = useState(false);

  const syncAccount = async () => {
    setAccountLoading(true);
    if (!initialAccountRoadmap || !initialLocalRoadmap) return;
    await saveRoadmap(isLoggedIn, initialAccountRoadmap?.planners ?? null, initialAccountRoadmap?.planners ?? null);
    setShowSyncModal(false);
  };

  const getRelativeTime = (timestamp: string | undefined) => {
    if (!timestamp) return undefined;
    const diff = new Date(timestamp).getTime() - Date.now();
    const seconds = Math.round(diff / 1000);
    const minutes = Math.round(seconds / 60);
    const hours = Math.round(minutes / 60);
    const days = Math.round(hours / 24);

    const rtf = new Intl.RelativeTimeFormat('en', { numeric: 'auto' });

    if (Math.abs(seconds) < 60) return rtf.format(seconds, 'second');
    if (Math.abs(minutes) < 60) return rtf.format(minutes, 'minute');
    if (Math.abs(hours) < 24) return rtf.format(hours, 'hour');
    return rtf.format(days, 'day');
  };

  const countRoadmapStats = (roadmap: SavedRoadmap) => {
    const lastEdited = roadmap?.timestamp ? new Date(roadmap.timestamp).toLocaleString() : undefined;
    const roadmapCount = roadmap.planners.length;
    const courseCount = roadmap.planners
      .flatMap((planner) => planner.content)
      .flatMap((year) => year.quarters)
      .reduce((total, quarter) => total + quarter.courses.length, 0);

    return { lastEdited, roadmapCount, courseCount };
  };

  // stats for synced roadmap
  const {
    lastEdited: syncedLastEdited,
    roadmapCount: syncedRoadmapCount,
    courseCount: syncedCourseCount,
  } = initialAccountRoadmap ? countRoadmapStats(initialAccountRoadmap) : { roadmapCount: 0, courseCount: 0 };

  const {
    lastEdited: localLastEdited,
    roadmapCount: localRoadmapCount,
    courseCount: localCourseCount,
  } = initialLocalRoadmap ? countRoadmapStats(initialLocalRoadmap) : { roadmapCount: 0, courseCount: 0 };

  return (
    <Dialog
      open={showSyncModal}
      onClose={() => {
        setShowSyncModal(false);
      }}
    >
      <DialogTitle>Roadmap Out of Sync</DialogTitle>
      <DialogContent>
        <DialogContentText>
          This device's saved roadmap has newer changes than the one saved to your account. Where would you like to load
          your roadmap from?
        </DialogContentText>
        <Divider sx={{ my: 4 }} />
        {/* Displayed Info */}
        <div
          className="displayed-info"
          style={{
            display: 'grid',
            gridTemplateColumns: '1fr 1fr',
            gap: '16px',
            marginTop: '12px',
            marginBottom: '12px',
          }}
        >
          <div style={{ display: 'flex', flexDirection: 'column' }}>
            <DialogContentText>
              <strong>This Device</strong>
            </DialogContentText>
            <DialogContentText>
              Last edited: <strong>{getRelativeTime(localLastEdited)}</strong>
            </DialogContentText>
            <DialogContentText>
              <strong>{localRoadmapCount}</strong> roadmaps
            </DialogContentText>
            <DialogContentText>
              <strong>{localCourseCount}</strong> courses
            </DialogContentText>
            <Button
              loading={overrideLoading}
              disabled={overrideLoading || accountLoading}
              color="error"
              variant="contained"
              onClick={overrideAccountRoadmap}
              sx={{ marginTop: 4, alignSelf: 'flex-start' }}
            >
              This Device
            </Button>
            <DialogContentText sx={{ marginTop: 2, fontSize: 12, color: 'red' }}>
              Warning: Loading from this device can override your synced account data.
            </DialogContentText>
          </div>

          <div style={{ display: 'flex', flexDirection: 'column' }}>
            <DialogContentText>
              <strong>My Account</strong>
            </DialogContentText>
            <DialogContentText>
              Last edited: <strong>{getRelativeTime(syncedLastEdited)}</strong>
            </DialogContentText>
            <DialogContentText>
              <strong>{syncedRoadmapCount}</strong> roadmaps
            </DialogContentText>
            <DialogContentText>
              <strong>{syncedCourseCount}</strong> courses
            </DialogContentText>
            <Button
              loading={accountLoading}
              disabled={overrideLoading || accountLoading}
              variant="contained"
              onClick={syncAccount}
              sx={{ marginTop: 4, alignSelf: 'flex-start' }}
            >
              My Account
            </Button>
          </div>
        </div>
      </DialogContent>
    </Dialog>
  );
};

export default PlannerLoader;
