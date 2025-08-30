// revision has multiple changes

import { RoadmapPlan } from '../store/slices/roadmapSlice';
import { PlannerQuarterData, PlannerYearData } from './types';

export type FullPlannerChangeData = Omit<RoadmapPlan, 'content'> | null;
export interface PlannerEdit {
  type: 'planner';
  before: FullPlannerChangeData;
  after: FullPlannerChangeData;
}

export type PlannerYearChangeData = Omit<PlannerYearData, 'quarters'> | null;
export interface PlannerYearEdit {
  type: 'year';
  plannerId: string;
  before: PlannerYearChangeData;
  after: PlannerYearChangeData;
}

export type PlannerQuarterChangeData = PlannerQuarterData | null;
export interface PlannerQuarterEdit {
  type: 'quarter';
  plannerId: string;
  startYear: number;
  before: PlannerQuarterChangeData;
  after: PlannerQuarterChangeData;
}

export type RoadmapEdit = PlannerEdit | PlannerYearEdit | PlannerQuarterEdit;
