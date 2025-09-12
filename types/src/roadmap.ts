import { z } from 'zod';

export const quarters = ['Fall', 'Winter', 'Spring', 'Summer1', 'Summer10wk', 'Summer2'] as const;

export const quarterName = z.enum(quarters);
export type QuarterName = z.infer<typeof quarterName>;

export const savedPlannerQuarterData = z.object({
  name: quarterName,
  courses: z.array(z.string()),
});
export type SavedPlannerQuarterData = z.infer<typeof savedPlannerQuarterData>;

export const savedPlannerYearData = z.object({
  startYear: z.number(),
  name: z.string().max(35),
  quarters: z.array(savedPlannerQuarterData),
});
export type SavedPlannerYearData = z.infer<typeof savedPlannerYearData>;

export const savedPlannerData = z.object({
  id: z.number(),
  name: z.string().max(35),
  content: z.array(savedPlannerYearData),
});
export type SavedPlannerData = z.infer<typeof savedPlannerData>;

// Specify name of transfer course and how many units its worth
export const legacyTransfer = z.object({
  name: z.string(),
  units: z.number().nullish(),
});
export type LegacyTransfer = z.infer<typeof legacyTransfer>;

/*
  An extended version of TransferData
  that optionally allows for a score (for AP exams from Zot4Plan)
*/
export const extendedTransferData = z.object({
  name: z.string(),
  units: z.number().nullish(),
  score: z.number().nullish(),
});
export type ExtendedTransferData = z.infer<typeof extendedTransferData>;

// Bundle planner and transfer data in one object
export const savedRoadmap = z.object({
  timestamp: z.string().optional(),
  planners: z.array(savedPlannerData),
  transfers: z.array(legacyTransfer).optional().describe('Used for legacy transfers only'),
});

export type SavedRoadmap = z.infer<typeof savedRoadmap>;

export interface LegacyRoadmap {
  planner: SavedPlannerYearData[];
  transfers: LegacyTransfer[];
  timestamp?: string;
}

// Roadmap Diffs

const roadmapPlannerYearChangeIdentifier = z.object({ plannerId: z.number().int() });

const roadmapPlannerQuarterChangeIdentifier = z.object({
  plannerId: z.number().int(),
  startYear: z.number().int(),
});

export function roadmapDeletionOf<S extends z.ZodRawShape>(zodType: z.ZodObject<S>) {
  return zodType.extend({ id: z.number().int() });
}

type RoadmapItemDeletion<T extends z.ZodRawShape> = z.infer<ReturnType<typeof roadmapDeletionOf<T>>>;
export type PlannerDeletion = RoadmapItemDeletion<Record<string, never>>;
export type PlannerYearDeletion = RoadmapItemDeletion<typeof roadmapPlannerYearChangeIdentifier.shape>;
export type PlannerQuarterDeletion = RoadmapItemDeletion<typeof roadmapPlannerQuarterChangeIdentifier.shape>;

const roadmapPlannerChange = z.object({
  data: z.object({ id: z.number().int(), name: z.string().max(35) }),
});

const roadmapPlannerYearChange = roadmapPlannerYearChangeIdentifier.extend({
  data: z.object({ startYear: z.number().int(), name: z.string().max(35) }),
});

const roadmapPlannerQuarterChange = roadmapPlannerQuarterChangeIdentifier.extend({
  data: z.object({ name: z.string().max(35) }),
});

export type RoadmapPlannerChange = z.infer<typeof roadmapPlannerChange>;
export type RoadmapPlannerYearChange = z.infer<typeof roadmapPlannerYearChange>;
export type RoadmapPlannerQuarterChange = z.infer<typeof roadmapPlannerQuarterChange>;

const plannerQuarterDiffs = z.object({
  updatedQuarters: z.array(roadmapPlannerQuarterChange),
});

const plannerYearDiffs = plannerQuarterDiffs.extend({
  deletedQuarters: z.array(roadmapDeletionOf(roadmapPlannerQuarterChangeIdentifier)),
  newQuarters: z.array(roadmapPlannerQuarterChange),
  updatedYears: z.array(roadmapPlannerYearChange),
});

const plannerDiffs = plannerYearDiffs.extend({
  deletedYears: z.array(roadmapDeletionOf(roadmapPlannerYearChangeIdentifier)),
  newYears: z.array(roadmapPlannerYearChange),
  updatedPlanners: z.array(roadmapPlannerChange),
});

/** @todo split into similar structure as frontend RoadmapDiff extends PlannerDiffs extends ... */
export const roadmapDiffs = plannerDiffs.extend({
  deletedPlanners: z.array(roadmapDeletionOf(z.object({}))),
  newPlanners: z.array(roadmapPlannerChange),
});

export type PlannerQuarterDiffs = z.infer<typeof plannerQuarterDiffs>;
export type PlannerYearDiffs = z.infer<typeof plannerYearDiffs>;
export type PlannerDiffs = z.infer<typeof plannerDiffs>;
export type RoadmapDiffs = z.infer<typeof roadmapDiffs>;
