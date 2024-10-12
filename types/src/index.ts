import { z } from 'zod';

export const reportSubmission = z.object({
  reviewID: z.string(),
  reason: z.string().min(1).max(500),
});
export type ReportSubmission = z.infer<typeof reportSubmission>;

export const reportData = reportSubmission.extend({
  _id: z.string(),
  timestamp: z.string(),
});
export type ReportData = z.infer<typeof reportData>;

export const theme = z.enum(['light', 'dark', 'system']);
export type Theme = z.infer<typeof theme>;

export const userPreferences = z.object({
  theme: theme,
});
export type UserPreferences = z.infer<typeof userPreferences>;

export * from 'peterportal-api-next-types';
