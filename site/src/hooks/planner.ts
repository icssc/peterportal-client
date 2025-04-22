import { getAllCoursesFromPlan } from '../helpers/planner';
import { useAppSelector } from '../store/hooks';

export function useClearedCourses() {
  const roadmap = useAppSelector((state) => state.roadmap);
  const allExistingCourses = getAllCoursesFromPlan(roadmap?.plans[roadmap.currentPlanIndex].content);
  const transfers = roadmap?.transfers.map((transfer) => transfer.name);
  const clearedCourses = new Set([...allExistingCourses, ...transfers]);
  return clearedCourses;
}
