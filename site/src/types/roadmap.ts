import { InvalidCourseData, PlannerData, PlannerQuarterData, PlannerYearData } from './types';

// Client-side Roadmaps

export interface RoadmapPlanState {
  yearPlans: PlannerData;
  /** Courses that do not meet their prerequisites */
  invalidCourses: InvalidCourseData[];
}

export interface RoadmapPlan {
  id?: number;
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
  id: string;
  edits: RoadmapEdit[];
}

export type RevisionDirection = 'undo' | 'redo';

export interface RevisionStack {
  edits: RoadmapEdit[];
  direction: RevisionDirection;
}
