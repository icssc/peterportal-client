/**
 @module CoursesRoute
*/

import { z } from 'zod';
import { getCourseQuery } from '../helpers/gql';
import { publicProcedure, router } from '../helpers/trpc';
import { GradesRaw } from '@peterportal/types';

const coursesRouter = router({
  /**
   * PPAPI proxy for getting course data
   */
  get: publicProcedure.input(z.object({ courseID: z.number() })).query(async ({ input }) => {
    const r = fetch(process.env.PUBLIC_API_URL + 'courses/' + encodeURIComponent(input.courseID), {
      headers: {
        'Content-Type': 'application/json',
        Accept: 'application/json',
      },
    });
    console.log(input.courseID);

    return r.then((response) => response.json()).then((data) => data.payload);
  }),

  /**
   * PPAPI proxy for batch course data
   */
  batch: publicProcedure.input(z.object({ courses: z.string().array() })).mutation(async ({ input }) => {
    if (input.courses.length == 0) {
      return {};
    } else {
      const r = fetch(process.env.PUBLIC_API_GRAPHQL_URL!, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          query: getCourseQuery(input.courses),
        }),
      });

      return r
        .then((response) => response.json())
        .then((data) =>
          Object.fromEntries(
            Object.values(data.data)
              .filter((x) => x !== null)
              .map((x) => [(x as { id: string }).id, x]),
          ),
        );
    }
  }),

  /**
   * PPAPI proxy for grade distribution
   */
  grades: publicProcedure.input(z.object({ department: z.string(), number: z.string() })).query(async ({ input }) => {
    const r = fetch(
      process.env.PUBLIC_API_URL +
        'grades/raw?department=' +
        encodeURIComponent(input.department) +
        '&courseNumber=' +
        input.number,
    );

    return r.then((response) => response.json()).then((data) => data.payload as GradesRaw);
  }),
});

export default coursesRouter;
