/**
 @module SearchRoute
*/

import { z } from 'zod';
import { publicProcedure, router } from '../helpers/trpc';
import { SearchAAPIResponse } from '@peterportal/types';

const searchRouter = router({
  /**
   * Anteater API proxy for fuzzy search
   */
  get: publicProcedure
    .input(
      z.object({
        query: z.string(),
        skip: z
          .number()
          .int()
          .transform((x) => x.toString()),
        take: z
          .number()
          .int()
          .transform((x) => x.toString()),
        resultType: z.union([z.literal('course'), z.literal('instructor')]),
      }),
    )
    .query(async ({ input }) => {
      const r = fetch(`${process.env.PUBLIC_API_URL}search?${new URLSearchParams(input).toString()}`, {
        headers: {
          'Content-Type': 'application/json',
          Accept: 'application/json',
          ...(process.env.ANTEATER_API_KEY && { Authorization: `Bearer ${process.env.ANTEATER_API_KEY}` }),
        },
      });

      return r.then((response) => response.json()).then((data) => data.data as SearchAAPIResponse);
    }),
});

export default searchRouter;
