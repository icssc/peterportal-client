import { quarters } from 'peterportal-api-next-types';
import { z } from 'zod';

export const quarterName = z.enum(quarters);
export type QuarterName = z.infer<typeof quarterName>;

// these aren't used anymore but some old roadmaps still have them, included for valid schema
export const legacyQuarterNames = z.enum([
  'fall',
  'winter',
  'spring',
  'summer I',
  'summer II',
  'summer 10 Week',
  'First Summer',
  'Second Summer',
  'Special / 10-Week Summer',
]);

export const savedPlannerQuarterData = z.object({
  name: quarterName.or(legacyQuarterNames),
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
  name: z.string().max(35),
  content: z.array(savedPlannerYearData),
});
export type SavedPlannerData = z.infer<typeof savedPlannerData>;

// Specify name of transfer course and how many units its worth
export const transferData = z.object({
  name: z.string(),
  units: z.number().optional(),
});
export type TransferData = z.infer<typeof transferData>;

// Bundle planner and transfer data in one object
export const savedRoadmap = z.object({
  timestamp: z.number(),
  planners: z.array(savedPlannerData),
  transfers: z.array(transferData),
});
export type SavedRoadmap = z.infer<typeof savedRoadmap>;

// Structure stored in mongo for accounts
export const mongoRoadmap = z.object({
  roadmap: savedRoadmap,
  userID: z.string(),
  coursebag: z.array(z.string()),
});
export type MongoRoadmap = z.infer<typeof mongoRoadmap>;
