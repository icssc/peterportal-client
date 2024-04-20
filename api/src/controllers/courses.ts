/**
 @module CoursesRoute
*/

import { getCourseQuery } from '../helpers/gql';
import { publicProcedure, router } from '../helpers/trpc';
import typia from 'typia';

const coursesRouter = router({
  /**
   * PPAPI proxy for getting course data
   */
  get: publicProcedure.input(typia.createAssert<{ courseID: number }>()).query(async ({ input }) => {
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
  batch: publicProcedure.input(typia.createAssert<{ courses: string[] }>()).mutation(async ({ input }) => {
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
  grades: publicProcedure
    .input(typia.createAssert<{ department: string; number: string }>())
    .query(async ({ input }) => {
      const r = fetch(
        process.env.PUBLIC_API_URL +
          'grades/raw?department=' +
          encodeURIComponent(input.department) +
          '&courseNumber=' +
          input.number,
      );

      return r.then((response) => response.json()).then((data) => data.payload);
    }),
});

export default coursesRouter;
