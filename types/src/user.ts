import { z } from 'zod';

export const theme = z.enum(['light', 'dark', 'system']);
export type Theme = z.infer<typeof theme>;

export const userPreferences = z.object({
  theme: theme,
});
export type UserPreferences = z.infer<typeof userPreferences>;

/** @todo make use of this on frontend when accessing user cookie with useCookies */
export interface User {
  id: string;
  email: string;
  name: string;
  picture: string;
}
