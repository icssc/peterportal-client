import { quarters } from '@peterportal/types';
import {
  FullPlannerChangeData,
  PlannerQuarterChangeData /* , PlannerYearChangeData */,
  PlannerYearChangeData,
  RevisionDirection,
  RevisionStack,
  RoadmapPlan,
  RoadmapRevision,
} from '../types/roadmap';

export const EMPTY_PLAN = {
  yearPlans: [],
  invalidCourses: [],
};

// Applying "revisions" to the roadmap

export function applyFullPlannerEdit(
  plans: RoadmapPlan[],
  oldData: FullPlannerChangeData,
  newData: FullPlannerChangeData,
): void {
  if (!oldData && !newData) return;

  if (!oldData) {
    plans.push({ ...newData!, content: { ...EMPTY_PLAN } });
    return;
  }

  const planIndex = plans.findIndex((plan) => plan.id === oldData.id);
  if (!newData) {
    plans.splice(planIndex, 1);
    return;
  }

  Object.assign(plans[planIndex], newData);
}

export function applyYearEdit(
  plans: RoadmapPlan[],
  plannerId: number,
  oldData: PlannerYearChangeData,
  newData: PlannerYearChangeData,
) {
  if (!oldData && !newData) return;

  const planToEdit = plans.find((plan) => plan.id === plannerId);
  if (!planToEdit) return;

  const plannerYears = planToEdit.content.yearPlans;

  if (!oldData) {
    plannerYears.push({ ...newData!, quarters: [] });
    plannerYears.sort((a, b) => a.startYear - b.startYear);
    return;
  }

  const yearIndex = plannerYears.findIndex((year) => year.startYear === oldData.startYear);
  if (!newData) {
    plannerYears.splice(yearIndex, 1);
    return;
  }

  Object.assign(plannerYears[yearIndex], newData);
  plannerYears.sort((a, b) => a.startYear - b.startYear);
}

export function applyQuarterEdit(
  plans: RoadmapPlan[],
  plannerId: number,
  startYear: number,
  oldData: PlannerQuarterChangeData,
  newData: PlannerQuarterChangeData,
) {
  if (!oldData && !newData) return;

  const planToEdit = plans.find((plan) => plan.id === plannerId);
  const yearToEdit = planToEdit?.content?.yearPlans?.find((year) => year.startYear === startYear);
  if (!yearToEdit) return;

  if (!oldData) {
    yearToEdit.quarters.push(newData!);
    yearToEdit.quarters.sort((a, b) => quarters.indexOf(a.name) - quarters.indexOf(b.name));
    return;
  }

  const quarterIndex = yearToEdit.quarters.findIndex((q) => q.name === oldData.name);
  if (!newData) {
    yearToEdit.quarters.splice(quarterIndex);
    return;
  }

  // the only data you can change about a quarter is the courses
  yearToEdit.quarters[quarterIndex].courses = newData.courses;
}

// Creating revisions for the roadmap

// Traversing the revision stack
function getRevisionStack(history: RoadmapRevision[], start: number, end: number): RevisionStack {
  // Track number of positions changed
  const steps = end - start;
  const first = Math.min(end, start) + 1;
  const last = Math.max(end, start) + 1;
  const direction: RevisionDirection = Math.sign(steps) === -1 ? 'undo' : 'redo';

  // How edits are grouped has no effect on how we apply them, so flatten revisions into their edits
  const edits = history.slice(first, last).flatMap((r) => r.edits.slice());

  // Reverse the order if needed; undo starts with latest and redo starts with earliest
  if (direction === 'undo') edits.reverse();

  return { edits, direction };
}

function updatePlannerFromRevisionStack(planners: RoadmapPlan[], stack: RevisionStack) {
  stack.edits.forEach((edit) => {
    const oldKey = stack.direction === 'undo' ? 'after' : 'before';
    const newKey = stack.direction === 'undo' ? 'before' : 'after';

    switch (edit.type) {
      case 'planner':
        return applyFullPlannerEdit(planners, edit[oldKey], edit[newKey]);
      case 'year':
        return applyYearEdit(planners, edit.plannerId, edit[oldKey], edit[newKey]);
      case 'quarter': {
        return applyQuarterEdit(planners, edit.plannerId, edit.startYear, edit[oldKey], edit[newKey]);
      }
    }
  });
}

/**
 * Applies the provided revisions in-place on the planners argument
 * @param start The index of the first revision to be applied
 * @param end The index of the last revision to be applied
 */
export function restoreRevision(
  planners: RoadmapPlan[],
  revisionHistory: RoadmapRevision[],
  start: number,
  end: number,
) {
  const stack = getRevisionStack(revisionHistory, start, end);
  updatePlannerFromRevisionStack(planners, stack);
}

// diffing

// backend: tables
// backend: applying diffs
