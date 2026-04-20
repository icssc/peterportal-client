import { useEffect } from 'react';
import { loadMarkerCompletion, loadOverriddenRequirements } from '../helpers/courseRequirements';
import { useIsLoggedIn } from './isLoggedIn';
import { useAppDispatch, useAppSelector } from '../store/hooks';
import { initializeCompletedMarkers, initializeOverriddenRequirements } from '../store/slices/courseRequirementsSlice';

export function useLoadCompletedMarkers() {
  const isLoggedIn = useIsLoggedIn();
  const dispatch = useAppDispatch();

  // Load user-related Degree Requirements data (as opposed to AAPI-provided data)
  useEffect(() => {
    loadMarkerCompletion(isLoggedIn).then((completedMarkers) => {
      dispatch(initializeCompletedMarkers(completedMarkers));
    });
  }, [dispatch, isLoggedIn]);
}

export function useLoadOveriddenRequirements() {
  const isLoggedIn = useIsLoggedIn();
  const dispatch = useAppDispatch();

  const plans = useAppSelector((state) => state.roadmap.plans);
  const planIndex = useAppSelector((state) => state.roadmap.currentPlanIndex);
  const activePlanID = plans[planIndex]?.id;

  useEffect(() => {
    if (!activePlanID) return;
    loadOverriddenRequirements(activePlanID, isLoggedIn).then((overriddenRequirements) => {
      dispatch(initializeOverriddenRequirements({ plannerId: activePlanID, requirements: overriddenRequirements }));
    });
  }, [dispatch, activePlanID, isLoggedIn]);
}
