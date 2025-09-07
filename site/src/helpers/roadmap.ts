import { quarters } from '@peterportal/types';
import {
  FullPlannerChangeData,
  PlannerDiffs,
  PlannerEdit,
  PlannerQuarterChangeData,
  PlannerQuarterDiffs,
  PlannerQuarterEdit,
  PlannerYearChangeData,
  PlannerYearDiffs,
  PlannerYearEdit,
  RevisionDirection,
  RevisionStack,
  RoadmapDiffs,
  RoadmapEdit,
  RoadmapEditIdentifier,
  RoadmapPlan,
  RoadmapRevision,
  RoadmapSaveInstruction,
} from '../types/roadmap';
import { PlannerQuarterData, PlannerYearData } from '../types/types';
import { deepCopy } from './util';

export const createEmptyPlan = () => ({
  yearPlans: [],
  invalidCourses: [],
});

// Applying "revisions" to the roadmap

export function applyFullPlannerEdit(
  plans: RoadmapPlan[],
  oldData: FullPlannerChangeData,
  newData: FullPlannerChangeData,
): void {
  if (!oldData && !newData) return;

  if (!oldData) {
    plans.push({ ...newData!, content: createEmptyPlan() });
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
    yearToEdit.quarters.splice(quarterIndex, 1);
    return;
  }

  // the only data you can change about a quarter is the courses
  yearToEdit.quarters[quarterIndex].courses = newData.courses;
}

// Traversing the revision stack
function getRevisionStack(history: RoadmapRevision[], start: number, end: number): RevisionStack {
  // Track number of positions changed
  const steps = end - start;
  const first = Math.min(end, start) + 1;
  const last = Math.max(end, start) + 1;
  const direction: RevisionDirection = Math.sign(steps) === -1 ? 'undo' : 'redo';

  // How edits are grouped has no effect on how we apply them, so flatten revisions into their edits
  const edits = history.slice(first, last).flatMap((r) => deepCopy(r.edits));

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

export function createRevision(edits: RoadmapEdit[]): RoadmapRevision {
  return { timestamp: Date.now(), edits };
}

// diffing

function findNotInOther<T>(otherList: T[], matchingKey: keyof T) {
  return (item: T) => !otherList.find((otherItem) => otherItem[matchingKey] === item[matchingKey]);
}

/**
 * Based on two lists of roadmap data (i.e. PlannerQuarters), returns which ones have been deleted from
 * the `before` list, are newly added to the `after` list, or exist in both lists.
 *
 * @param before The list of contained items from before making changes, i.e. old quarters data
 * inside of a planner year
 * @param after The list of contained items from after making changes, i.e. new quarters data
 * inside of a planner year
 * @param itemIdKey The key used to identify items within the previously mentioned lists, i.e.
 * "name" if `before` and `after` are lists of PlannerQuarter
 * @param parentIdentifier The set of keys needed to define where the edit occurred, i.e. the
 * plannerId of the planner and startYear of the year that contains quarters being compared.
 * @returns Lists of removed items, added items, and matching items based on the `before` and `after` lists
 */
function getDiffsAndPairs<EditType extends RoadmapEdit, C extends Exclude<EditType['after'], null>>(
  before: C[],
  after: C[],
  itemIdKey: keyof C,
  parentIdentifier: RoadmapEditIdentifier<EditType>,
) {
  const removed: RoadmapSaveInstruction<EditType, false>[] = before
    .filter(findNotInOther(after, itemIdKey))
    .map((year) => ({ ...parentIdentifier, id: year[itemIdKey] as number }));

  const added: RoadmapSaveInstruction<EditType, true>[] = after
    .filter(findNotInOther(before, itemIdKey))
    .map((year) => ({ ...parentIdentifier, data: year }));

  const pairs = after.map((year) => [before.find((y) => y[itemIdKey] === year[itemIdKey]) ?? null, year] as const);

  return { removed, added, pairs };
}

/**
 * Adds the quarter being compared to the `updatedQuarters` list if the courses do not match exactly
 */
function comparePlannerQuarterPair(
  before: PlannerQuarterData | null,
  after: PlannerQuarterData,
  plannerId: number,
  startYear: number,
  plannerDiffs: PlannerQuarterDiffs,
) {
  if (!before) return;

  /** @todo update logic here after adding support for variable units */
  const hasSameCourses =
    before.courses.every((course, index) => course.id === after.courses[index]?.id) &&
    after.courses.every((course, index) => course.id === before.courses[index]?.id);
  if (hasSameCourses) return;

  const quarterUpdate = { name: after.name, courses: after.courses };
  plannerDiffs.updatedQuarters.push({ plannerId, startYear, data: quarterUpdate });
}

/**
 * Given the before and after state of a Planner Year, this function lists which quarters have been added,
 * deleted, or modified.
 * @param before The previous planner year, if it exists. Similar to `comparePlannerPair`, calling with `null`
 * will occur when we need to create new quarters for a "to-be-created" planner year.
 */
function comparePlannerYearPair(
  before: PlannerYearData | null,
  after: PlannerYearData,
  plannerId: number,
  plannerDiffs: PlannerYearDiffs,
) {
  const yearEditsIdentifier: RoadmapEditIdentifier<PlannerQuarterEdit> = { plannerId, startYear: after.startYear };
  const { removed, added, pairs } = getDiffsAndPairs(
    before?.quarters ?? [],
    after.quarters,
    'name',
    yearEditsIdentifier,
  );

  pairs.forEach(([oldYear, newYear]) =>
    comparePlannerQuarterPair(oldYear, newYear, plannerId, after.startYear, plannerDiffs),
  );

  if (before && before.name !== after.name) {
    const yearUpdate = { name: after.name, startYear: after.startYear };
    plannerDiffs.updatedYears.push({ data: yearUpdate, plannerId });
  }

  plannerDiffs.deletedQuarters.push(...removed);
  plannerDiffs.newQuarters.push(...added);
}

/**
 * Given the before and after state of a Planner, this function lists which years have been added, deleted,
 * or modified. In doing so, it will also list changes associated wtih specific quarters in that Planner Year.
 * @param before The previous planner data, if it exists. A call to `comparePlannerPair` with `before = null`
 * must occur in order to create years for a planner that will exist after save (but has not been created yet).
 */
function comparePlannerPair(before: RoadmapPlan | null, after: RoadmapPlan, plannerDiffs: PlannerDiffs) {
  const beforeYears = before?.content?.yearPlans ?? [];
  const afterYears = after.content.yearPlans;

  const yearEditsIdentifier: RoadmapEditIdentifier<PlannerYearEdit> = { plannerId: after.id };
  const { removed, added, pairs } = getDiffsAndPairs(beforeYears, afterYears, 'startYear', yearEditsIdentifier);

  pairs.forEach(([oldYear, newYear]) => comparePlannerYearPair(oldYear, newYear, after.id, plannerDiffs));

  if (before && before.name !== after.name) {
    const plannerUpdate = { id: after.id, name: after.name };
    plannerDiffs.updatedPlanners.push({ data: plannerUpdate });
  }

  plannerDiffs.deletedYears.push(...removed);
  plannerDiffs.newYears.push(...added);
}

/**
 * Generates a list of changes to all user Roadmaps, given a before and after state.
 * @returns Lists of creations, deletions, and modifications for planners, years, and quarters. Database
 * updates using these changes should be performed as such: deletes from small (quarter) to large (planner),
 * then modifications from small to large, then creates from large to small.
 */
export function compareRoadmaps(before: RoadmapPlan[], after: RoadmapPlan[]): RoadmapDiffs {
  const roadmapEditsIdentifier: RoadmapEditIdentifier<PlannerEdit> = {};
  const {
    removed: deletedPlanners,
    added: newPlanners,
    pairs: matchingPlanners,
  } = getDiffsAndPairs(before, after, 'id', roadmapEditsIdentifier);

  const updatedPlanners: RoadmapSaveInstruction<PlannerEdit, true>[] = [];
  const updatedYears: RoadmapSaveInstruction<PlannerYearEdit, true>[] = [];
  const updatedQuarters: RoadmapSaveInstruction<PlannerQuarterEdit, true>[] = [];
  const newYears: RoadmapSaveInstruction<PlannerYearEdit, true>[] = [];
  const newQuarters: RoadmapSaveInstruction<PlannerQuarterEdit, true>[] = [];
  const deletedYears: RoadmapSaveInstruction<PlannerYearEdit, false>[] = [];
  const deletedQuarters: RoadmapSaveInstruction<PlannerQuarterEdit, false>[] = [];

  const plannerDiffs: PlannerDiffs = {
    deletedQuarters,
    deletedYears,
    updatedQuarters,
    updatedYears,
    updatedPlanners,
    newYears,
    newQuarters,
  };

  matchingPlanners.forEach(([before, after]) => comparePlannerPair(before, after, plannerDiffs));

  return {
    deletedPlanners,
    newPlanners,
    ...plannerDiffs,
  };
}

// backend: tables
// backend: applying diffs
