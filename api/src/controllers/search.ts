/**
 @module SearchRoute
*/

import { z } from 'zod';
import { publicProcedure, router } from '../helpers/trpc';
import { SearchAAPIResponse } from '@peterportal/types';
import { ANTEATER_API_REQUEST_HEADERS } from '../helpers/headers';

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
        headers: ANTEATER_API_REQUEST_HEADERS,
      });

      return r.then((response) => response.json()).then((data) => data.data as SearchAAPIResponse);
    }),
});

export default searchRouter;
