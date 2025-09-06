import { QuarterName } from '@peterportal/types';
import { PlannerEdit, PlannerQuarterEdit, PlannerYearEdit, RoadmapPlan, RoadmapRevision } from '../types/roadmap';
import { CourseGQLData, PlannerQuarterData, PlannerYearData } from '../types/types';
import { createRevision } from './roadmap';

// [action][Type][Property]
// Examples:
// addPlanner, removePlanner, updatePlannerName
// addQuarter, removeQuarter, updateQuarterCourses

function createInverseRevision(revision: RoadmapRevision) {
  revision.edits.forEach((edit) => {
    const before = edit.before;
    edit.before = edit.after;
    edit.after = before;
  });
  revision.edits.reverse();
  return revision;
}

export function addPlanner(id: number, name: string, yearPlans?: PlannerYearData[]) {
  const plannerEdit: PlannerEdit = {
    type: 'planner',
    before: null,
    after: { id, name },
  };
  if (!yearPlans) return createRevision([plannerEdit]);

  const otherEdits = yearPlans
    .flatMap((year) => addPlannerYear(id, year.startYear, year.name, year.quarters))
    .flatMap((revision) => revision.edits);

  return createRevision([plannerEdit, ...otherEdits]);
}

export function deletePlanner(id: number, name: string, yearPlans?: PlannerYearData[]) {
  return createInverseRevision(addPlanner(id, name, yearPlans));
}

export function updatePlannerName(current: RoadmapPlan, newName: string) {
  const edit: PlannerEdit = {
    type: 'planner',
    before: { id: current.id, name: current.name },
    after: { id: current.id, name: newName },
  };
  return createRevision([edit]);
}

export function addPlannerYear(plannerId: number, startYear: number, name: string, quarters?: PlannerQuarterData[]) {
  const yearEdit: PlannerYearEdit = {
    type: 'year',
    plannerId,
    before: null,
    after: { name, startYear },
  };
  if (!quarters) return createRevision([yearEdit]);

  const otherEdits = quarters
    .flatMap((quarter) => addPlannerQuarter(plannerId, startYear, quarter.name, quarter.courses))
    .flatMap((revision) => revision.edits);

  return createRevision([yearEdit, ...otherEdits]);
}

export function addPlannerQuarter(plannerId: number, startYear: number, name: QuarterName, courses: CourseGQLData[]) {
  const edit: PlannerQuarterEdit = {
    type: 'quarter',
    plannerId,
    startYear,
    before: null,
    after: { name, courses },
  };

  return createRevision([edit]);
}
