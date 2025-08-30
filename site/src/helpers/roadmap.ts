import { quarters } from '@peterportal/types';
import { RoadmapPlan } from '../store/slices/roadmapSlice';
import {
  FullPlannerChangeData,
  PlannerQuarterChangeData /* , PlannerYearChangeData */,
  PlannerYearChangeData,
} from '../types/roadmap';

export const EMPTY_PLAN = {
  yearPlans: [],
  invalidCourses: [],
};

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
