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

/**
 * Save instruction is designed to be compatible with RoadmapItemDeletion and Roadmap[Item]Change
 * in /types/src/roadmap.ts
 *
 * However, it must still be its own type in /site/src/types because we need to read/traverse client-only
 * types (the ones that define nested data such as `quarters` on PlannerYearData) in order to create the
 * flattened diff.
 */
export type RoadmapSaveInstruction<T extends RoadmapEdit, Write extends boolean> = RoadmapEditIdentifier<T> &
  (Write extends true ? { data: Exclude<T['after'], null> } : { id: number });
