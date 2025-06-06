import { useMemo } from 'react';
import { useAppSelector } from '../store/hooks';
import { useTransferredCredits } from './transferCredits';
import { getNamesOfTransfers } from '../helpers/transferCredits';
import { getAllCoursesFromPlan } from '../helpers/planner';

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
