import { z } from 'zod';

export const theme = z.enum(['light', 'dark', 'system']);
export type Theme = z.infer<typeof theme>;

/** @todo make use of this on frontend when accessing user cookie with useCookies */
export interface PassportUser {
  /**
   * google id
   */
  id: string;
  email: string;
  name: string;
  picture: string;
}

export interface UserData extends Omit<PassportUser, 'id'> {
  id: number;
  theme: Theme;
  isAdmin: boolean;
  lastRoadmapEditAt?: string;
}
