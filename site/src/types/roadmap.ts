import { InvalidCourseData, PlannerQuarterData, PlannerYearData } from './types';

// Client-side Roadmaps

export interface RoadmapPlanState {
  yearPlans: PlannerYearData[];
  /** Courses that do not meet their prerequisites */
  invalidCourses: InvalidCourseData[];
}

export interface RoadmapPlan {
  id: number;
  name: string;
  content: RoadmapPlanState;
}

// Indvidual Changes
export type FullPlannerChangeData = Omit<RoadmapPlan, 'content'> | null;
export interface PlannerEdit {
  type: 'planner';
  before: FullPlannerChangeData;
  after: FullPlannerChangeData;
}

export type PlannerYearChangeData = Omit<PlannerYearData, 'quarters'> | null;
export interface PlannerYearEdit {
  type: 'year';
  plannerId: number;
  before: PlannerYearChangeData;
  after: PlannerYearChangeData;
}

export type PlannerQuarterChangeData = PlannerQuarterData | null;
export interface PlannerQuarterEdit {
  type: 'quarter';
  plannerId: number;
  startYear: number;
  before: PlannerQuarterChangeData;
  after: PlannerQuarterChangeData;
}

export type RoadmapEdit = PlannerEdit | PlannerYearEdit | PlannerQuarterEdit;

// Revisions have multiple changes

export interface RoadmapRevision {
  timestamp: number;
  edits: RoadmapEdit[];
}

export type RevisionDirection = 'undo' | 'redo';

export interface RevisionStack {
  edits: RoadmapEdit[];
  direction: RevisionDirection;
}

// Save Instructions

export type RoadmapEditIdentifier<T extends RoadmapEdit> = Omit<T, 'type' | 'before' | 'after'>;

export type RoadmapSaveInstruction<T extends RoadmapEdit, Write extends boolean> = RoadmapEditIdentifier<T> &
  (Write extends true ? { data: T['after'] } : { id: number });

export interface PlannerQuarterDiffs {
  updatedQuarters: RoadmapSaveInstruction<PlannerQuarterEdit, true>[];
}

export interface PlannerYearDiffs extends PlannerQuarterDiffs {
  deletedQuarters: RoadmapSaveInstruction<PlannerQuarterEdit, false>[];
  newQuarters: RoadmapSaveInstruction<PlannerQuarterEdit, true>[];
  updatedYears: RoadmapSaveInstruction<PlannerYearEdit, true>[];
}

export interface PlannerDiffs extends PlannerYearDiffs {
  deletedYears: RoadmapSaveInstruction<PlannerYearEdit, false>[];
  newYears: RoadmapSaveInstruction<PlannerYearEdit, true>[];
  updatedPlanners: RoadmapSaveInstruction<PlannerEdit, true>[];
}

export interface RoadmapDiffs extends PlannerDiffs {
  deletedPlanners: RoadmapSaveInstruction<PlannerEdit, false>[];
  newPlanners: RoadmapSaveInstruction<PlannerEdit, true>[];
}
