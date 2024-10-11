import { z } from 'zod';

export const reportData = z.object({
  reviewID: z.string(),
  reason: z.string(),
  timestamp: z.string(),
});
export type ReportData = z.infer<typeof reportData>;

export const theme = z.enum(['light', 'dark', 'system']);
export type Theme = z.infer<typeof theme>;

export const userPreferences = z.object({
  theme: theme,
});
export type UserPreferences = z.infer<typeof userPreferences>;
