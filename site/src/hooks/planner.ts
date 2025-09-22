import { useEffect, useMemo, useState } from 'react';
import { collapseAllPlanners, getAllCoursesFromPlan, saveRoadmap } from '../helpers/planner';
import { useAppDispatch, useAppSelector } from '../store/hooks';
import { useTransferredCredits } from './transferCredits';
import { getNamesOfTransfers } from '../helpers/transferCredits';
import { useIsLoggedIn } from './isLoggedIn';
import { RoadmapRevision } from '../types/roadmap';
import { reviseRoadmap, setSavedRevisionIndex } from '../store/slices/roadmapSlice';
import { deepCopy } from '../helpers/util';
import { restoreRevision } from '../helpers/roadmap';

export function useClearedCourses() {
  const { courses, ap, apInfo } = useTransferredCredits();

  const roadmap = useAppSelector((state) => state.roadmap);
  const allExistingCourses = getAllCoursesFromPlan(roadmap?.plans[roadmap.currentPlanIndex].content);
  const transfers = useMemo(() => getNamesOfTransfers(courses, ap, apInfo), [ap, apInfo, courses]);

  const clearedCourses = useMemo(() => {
    return new Set([...allExistingCourses, ...transfers]);
  }, [allExistingCourses, transfers]);

  return clearedCourses;
}

export function useSaveRoadmap() {
  const dispatch = useAppDispatch();
  const isLoggedIn = useIsLoggedIn();
  const planners = useAppSelector((state) => state.roadmap.plans);
  const revisions = useAppSelector((state) => state.roadmap.revisions);
  const currIdx = useAppSelector((state) => state.roadmap.currentRevisionIndex);
  const lastSaveIdx = useAppSelector((state) => state.roadmap.savedRevisionIndex);

  const handler = async (showToasts: boolean) => {
    // generate before and after from the current state
    const lastSavedRoadmapPlans = deepCopy(planners);
    restoreRevision(lastSavedRoadmapPlans, revisions, currIdx, lastSaveIdx);
    const collapsedPrevious = collapseAllPlanners(lastSavedRoadmapPlans);
    const collapsedCurrent = collapseAllPlanners(planners);

    await saveRoadmap(isLoggedIn, collapsedPrevious, collapsedCurrent, showToasts);
    dispatch(setSavedRevisionIndex(currIdx));
  };

  return handler;
}

export function useReviseAndSaveRoadmap() {
  const saveRoadmap = useSaveRoadmap();
  const dispatch = useAppDispatch();
  const revisions = useAppSelector((state) => state.roadmap.revisions);
  const currIdx = useAppSelector((state) => state.roadmap.currentRevisionIndex);
  const currentRevision = revisions[currIdx];

  const [expectedTimestamp, setExpectedTimestamp] = useState(-1);
  const [showToasts, setShowToasts] = useState(false);

  useEffect(() => {
    if (currentRevision?.timestamp !== expectedTimestamp) return;
    // This save can only be triggered by a change in either the `currentRevision`
    // or a change in expectedTimestamp. The two are only the same when the following
    // sequence happens (in order):
    // 1. expectedTimestamp gets updated to the revision about to be added
    // 2. That same expected revision gets added, making currentRevision.timestamp the same as expected
    // No other revision or expectedTimestamp should cause the two values to be the same
    // This proof is left as an exercise to the reader.
    setExpectedTimestamp(-1);
    saveRoadmap(showToasts);
  }, [currentRevision?.timestamp, expectedTimestamp, saveRoadmap, showToasts]);

  const handler = (revision: RoadmapRevision, showToasts: boolean) => {
    setExpectedTimestamp(revision.timestamp);
    setShowToasts(showToasts);
    dispatch(reviseRoadmap(revision));
  };

  return handler;
}
