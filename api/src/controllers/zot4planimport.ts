/**
 @module Zot4PlanImportRoute
*/

import { z } from 'zod';
import { publicProcedure, router } from '../helpers/trpc';
import { SavedRoadmap } from '@peterportal/types';

/**
 * Get a JSON schedule from Zot4Plan by name
 */
/*const getFromZot4Plan = async (scheduleName: string) => {
  // TODO: implement
  return {};
};*/

const zot4PlanImportRouter = router({
  /**
   * Get a roadmap formatted for PeterPortal based on a Zot4Plan schedule by name
   */
  getScheduleFormatted: publicProcedure.input(z.object({ scheduleName: z.string() })).query(async ({ input }) => {
    // Get the raw schedule data
    //const originalScheduleRaw = await getFromZot4Plan(input.scheduleName);
    // Convert it to the PeterPortal roadmap format
    // TODO: implement
    // TODO: verify that it's valid
    // Example:
    const resExample: SavedRoadmap = {
      planners: [
        {
          name: input.scheduleName,
          content: [
            {
              startYear: 2025,
              name: 'Example Year 1',
              quarters: [
                {
                  name: 'Fall',
                  courses: ['I&CSCI10'],
                },
              ],
            },
          ],
        },
      ],
      transfers: [
        {
          name: 'AP COMP SCI A',
          units: 4,
        },
      ],
    };
    // Return it
    return resExample as SavedRoadmap;
  }),
});

export default zot4PlanImportRouter;
