import { quarters } from 'peterportal-api-next-types';
import { z } from 'zod';

export const quarterName = z.enum(quarters);
/** @todo duplicate of Quarter from peterportal-api-next-types. probably don't need but also don't know if new api has a types package */
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
  id: z.number().optional(),
  name: z.string().max(35),
  content: z.array(savedPlannerYearData),
});
export type SavedPlannerData = z.infer<typeof savedPlannerData>;

// Specify name of transfer course and how many units its worth
export const transferData = z.object({
  name: z.string(),
  units: z.number().nullish(),
});
export type TransferData = z.infer<typeof transferData>;

// Bundle planner and transfer data in one object
export const savedRoadmap = z.object({
  timestamp: z.string().optional(),
  planners: z.array(savedPlannerData),
  transfers: z.array(transferData),
});
export type SavedRoadmap = z.infer<typeof savedRoadmap>;

// Structure stored in mongo for accounts
export const mongoRoadmap = z.object({
  roadmap: savedRoadmap,
  userId: z.number(),
  coursebag: z.array(z.string()),
});
export type MongoRoadmap = z.infer<typeof mongoRoadmap>;
