/**
 @module ProfessorsRoute
*/

import { z } from 'zod';
import { publicProcedure, router } from '../helpers/trpc';
import { GradesRaw, ProfessorAAPIResponse, ProfessorBatchAAPIResponse } from '@peterportal/types';

const professorsRouter = router({
  /**
   * Anteater API proxy for getting professor data
   */
  get: publicProcedure.input(z.object({ ucinetid: z.string() })).query(async ({ input }) => {
    const r = fetch(`${process.env.PUBLIC_API_URL}instructors/${input.ucinetid}`, {
      headers: {
        'Content-Type': 'application/json',
        Accept: 'application/json',
        ...(process.env.ANTEATER_API_KEY && { Authorization: `Bearer ${process.env.ANTEATER_API_KEY}` }),
      },
    });

    return r.then((response) => response.json()).then((data) => data.data as ProfessorAAPIResponse);
  }),

  /**
   * Anteater API proxy for batch professor data
   */
  batch: publicProcedure.input(z.object({ professors: z.array(z.string()) })).mutation(async ({ input }) => {
    if (input.professors.length == 0) {
      return {};
    } else {
      const r = fetch(
        `${process.env.PUBLIC_API_URL}instructors/batch?ucinetids=${input.professors.map(encodeURIComponent).join(',')}`,
        {
          headers: {
            'Content-Type': 'application/json',
            Accept: 'application/json',
            ...(process.env.ANTEATER_API_KEY && { Authorization: `Bearer ${process.env.ANTEATER_API_KEY}` }),
          },
        },
      );

      return r
        .then((response) => response.json())
        .then(
          (data: { data: ProfessorAAPIResponse[] }) =>
            Object.fromEntries(data.data.map((x) => [x.ucinetid, x])) as ProfessorBatchAAPIResponse,
        );
    }
  }),

  /**
   * Anteater API proxy for grade distribution
   */
  grades: publicProcedure.input(z.object({ name: z.string() })).query(async ({ input }) => {
    const r = fetch(`${process.env.PUBLIC_API_URL}grades/raw?instructor=${encodeURIComponent(input.name)}`);

    return r.then((response) => response.json()).then((data) => data.data as GradesRaw);
  }),
});

export default professorsRouter;
