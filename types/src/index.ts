import { z } from 'zod';

export const reportData = z.object({
  _id: z.string(),
  reviewID: z.string(),
  reason: z.string().min(1).max(500),
  timestamp: z.string(),
});
export type ReportData = z.infer<typeof reportData>;

export const reportSubmission = reportData.omit({ _id: true, timestamp: true });
export type ReportSubmission = z.infer<typeof reportSubmission>;

export const theme = z.enum(['light', 'dark', 'system']);
export type Theme = z.infer<typeof theme>;

export const userPreferences = z.object({
  theme: theme,
});
export type UserPreferences = z.infer<typeof userPreferences>;

export * from 'peterportal-api-next-types';
