import { useEffect, useRef, useMemo } from 'react';
import { getAllCoursesFromPlan } from '../helpers/planner';
import { useAppSelector } from '../store/hooks';
import { useTransferredCredits } from './transferCredits';
import { getNamesOfTransfers } from '../helpers/transferCredits';

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

export function useToggleRef(isMobile: boolean, show: boolean) {
  const overlayRef = useRef<HTMLDivElement>(null);
  const sidebarRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (!isMobile) return;
    sidebarRef.current?.classList.toggle('enter-done', show);
    overlayRef.current?.classList.toggle('enter-done', show);
  }, [isMobile, show]);

  return { overlayRef, sidebarRef };
}
