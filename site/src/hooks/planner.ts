import { useMemo } from 'react';
import { getAllCoursesFromPlan } from '../helpers/planner';
import { useAppSelector } from '../store/hooks';
import { useTransferredCredits } from './transferCredits';

export function useClearedCourses() {
  const roadmap = useAppSelector((state) => state.roadmap);
  const allExistingCourses = getAllCoursesFromPlan(roadmap?.plans[roadmap.currentPlanIndex].content);
  const transfers = useTransferredCredits().courses.map((c) => c.courseName);

  const clearedCourses = useMemo(() => {
    return new Set([...allExistingCourses, ...transfers]);
  }, [allExistingCourses, transfers]);

  return clearedCourses;
}
