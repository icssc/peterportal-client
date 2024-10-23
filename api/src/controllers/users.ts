/**
 @module UsersRoute
*/

import Preference from '../models/preference';
import { publicProcedure, router, userProcedure } from '../helpers/trpc';
import { userPreferences, UserPreferences } from '@peterportal/types';

const usersRouter = router({
  /**
   * Get the user's session data
   */
  get: publicProcedure.query(async ({ ctx }) => {
    return ctx.session;
  }),

  /**
   * Get the user's theme preferences
   */
  getPreferences: userProcedure.query(async ({ ctx }): Promise<UserPreferences> => {
    const userID = ctx.session.passport?.user.id;
    const preference = await Preference.findOne({ userID: userID });

    return (preference ? preference : {}) as UserPreferences;
  }),

  /**
   * Configure the user's theme preferences
   */
  setPreferences: userProcedure.input(userPreferences).mutation(async ({ input, ctx }) => {
    const userID = ctx.session.passport?.user.id;

    // make user's preference doc if it doesn't exist
    if (!(await Preference.exists({ userID }))) {
      await Preference.create({ userID, theme: input.theme });
    }

    // set the preferences
    await Preference.updateOne({ userID }, input);

    // echo back body
    return input;
  }),

  /**
   * Get whether or not a user is an admin
   */
  isAdmin: publicProcedure.query(async ({ ctx }) => {
    // not logged in
    if (!ctx.session?.passport) {
      return { admin: false };
    } else {
      return { admin: ctx.session.passport.isAdmin as boolean };
    }
  }),
});

export default usersRouter;
