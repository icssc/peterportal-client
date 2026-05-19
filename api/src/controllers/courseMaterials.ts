/**
 @module CourseMaterialsRoute
*/

import { z } from 'zod';
import { publicProcedure, router } from '../helpers/trpc';
import { CourseMaterialsAAPIResponse } from '@peterportal/types';
import { ANTEATER_API_REQUEST_HEADERS } from '../helpers/headers';

const CourseMaterialsRouter = router({
  /**
   * Anteater API proxy for getting course materials data
   */
  get: publicProcedure
    .input(z.object({ department: z.string(), courseNumber: z.string() }))
    .query(async ({ input }) => {
      const url = `${process.env.PUBLIC_API_URL}courseMaterials?department=${encodeURIComponent(input.department)}&courseNumber=${encodeURIComponent(input.courseNumber)}`;
      const response = await fetch(url, { headers: ANTEATER_API_REQUEST_HEADERS })
        .then((res) => res.json())
        .then((res) => {
          if (!res?.ok) return [];
          return (res.data as CourseMaterialsAAPIResponse) ?? [];
        });
      return response;
    }),
});

export default CourseMaterialsRouter;
