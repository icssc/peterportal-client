/**
 @module UsersRoute
*/

import typia from 'typia';

import Preference from '../models/preference';
import { publicProcedure, router, userProcedure } from '../helpers/trpc';

interface UserPreferences {
  theme?: 'light' | 'dark' | 'system';
}

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
  getPreferences: userProcedure.query(async ({ ctx }) => {
    const userID = ctx.session.passport?.user.id;
    const preference = await Preference.findOne({ userID: userID });

    return preference ? preference : { error: 'No preferences found' };
  }),

  /**
   * Configure the user's theme preferences
   */
  setPreferences: userProcedure.input(typia.createAssert<UserPreferences>()).mutation(async ({ input, ctx }) => {
    const userID = ctx.session.passport?.user.id;

    // make user's preference doc if it doesn't exist
    if (!(await Preference.exists({ userID }))) {
      await Preference.create({ userID, theme: input.theme });
    }

    // grab valid preferences from request body
    const preferences: UserPreferences = {};
    if (input.theme) {
      preferences.theme = input.theme;
    }

    // set the preferences
    await Preference.updateOne({ userID }, preferences);

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
      return { admin: ctx.session.passport.admin as boolean };
    }
  }),
});

export default usersRouter;
