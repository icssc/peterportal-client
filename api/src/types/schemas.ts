import { z } from 'zod';

export const reportData = z.object({
  reviewID: z.string(),
  reason: z.string(),
  timestamp: z.string(),
});

export type ReportData = z.infer<typeof reportData>;

export const userPreferences = z.object({
  theme: z.enum(['light', 'dark', 'system']),
});

export type UserPreferences = z.infer<typeof userPreferences>;
