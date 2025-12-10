import { z } from 'zod';

export const theme = z.enum(['light', 'dark', 'system']);
export type Theme = z.infer<typeof theme>;

export interface User {
  /**
   * google id
   */
  id: string;
  email: string;
  name: string;
  picture: string;
}

export interface UserData extends Omit<User, 'id'> {
  id: number;
  theme: Theme;
  isAdmin: boolean;
  lastRoadmapEditAt?: string;
}

export interface UserSliceState {
  user: Omit<User, 'id'> | null;
  theme: Theme | null;
  isAdmin: boolean;
}
