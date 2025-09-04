import { PlannerEdit, RoadmapPlan } from '../types/roadmap';
import { createRevision } from './roadmap';

// [action][Type][Property]
// Examples:
// addPlanner, removePlanner, updatePlannerName
// addQuarter, removeQuarter, updateQuarterCourses

export function updatePlannerName(current: RoadmapPlan, newName: string) {
  const edit: PlannerEdit = {
    type: 'planner',
    before: { id: current.id, name: current.name },
    after: { id: current.id, name: newName },
  };
  return createRevision([edit]);
}
