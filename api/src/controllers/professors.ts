/**
 @module ProfessorsRoute
*/

import { z } from 'zod';
import { getProfessorQuery } from '../helpers/gql';
import { publicProcedure, router } from '../helpers/trpc';
import { GradesRaw } from '@peterportal/types';

const professorsRouter = router({
  /**
   * PPAPI proxy for getting professor data
   */
  get: publicProcedure.input(z.object({ ucinetid: z.string() })).query(async ({ input }) => {
    const r = fetch(process.env.PUBLIC_API_URL + 'instructors/' + input.ucinetid);

    return r.then((response) => response.json());
  }),

  /**
   * PPAPI proxy for batch professor data
   */
  batch: publicProcedure.input(z.object({ professors: z.array(z.string()) })).mutation(async ({ input }) => {
    if (input.professors.length == 0) {
      return {};
    } else {
      const r = fetch(process.env.PUBLIC_API_GRAPHQL_URL!, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          query: getProfessorQuery(input.professors),
        }),
      });

      return r.then((response) => response.json()).then((data) => data.data);
    }
  }),

  /**
   * PPAPI proxy for grade distribution
   */
  grades: publicProcedure.input(z.object({ name: z.string() })).query(async ({ input }) => {
    const r = fetch(process.env.PUBLIC_API_URL + 'grades/raw?instructor=' + encodeURIComponent(input.name));

    return r
      .then((response) => {
        return response.json();
      })
      .then((data) => data.payload as GradesRaw);
  }),
});

export default professorsRouter;
