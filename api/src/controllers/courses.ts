/**
 @module CoursesRoute
*/

import { z } from 'zod';
import { publicProcedure, router } from '../helpers/trpc';
import { CourseAAPIResponse, CourseBatchAAPIResponse, GradesRaw } from '@peterportal/types';

const coursesRouter = router({
  /**
   * Anteater API proxy for getting course data
   */
  get: publicProcedure.input(z.object({ courseID: z.string() })).query(async ({ input }) => {
    const r = fetch(`${process.env.PUBLIC_API_URL}courses/${encodeURIComponent(input.courseID)}`, {
      headers: {
        'Content-Type': 'application/json',
        Accept: 'application/json',
        ...(process.env.ANTEATER_API_KEY && { Authorization: `Bearer ${process.env.ANTEATER_API_KEY}` }),
      },
    });

    return r.then((response) => response.json()).then((data) => data.data as CourseAAPIResponse);
  }),

  /**
   * Anteater API proxy for batch course data
   */
  batch: publicProcedure.input(z.object({ courses: z.string().array() })).mutation(async ({ input }) => {
    if (input.courses.length == 0) {
      return {};
    } else {
      const r = fetch(
        `${process.env.PUBLIC_API_URL}courses/batch?ids=${input.courses.map(encodeURIComponent).join(',')}`,
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
          (data: { data: CourseAAPIResponse[] }) =>
            Object.fromEntries(data.data.map((x) => [x.id, x])) as CourseBatchAAPIResponse,
        );
    }
  }),

  /**
   * Anteater API proxy for grade distribution
   */
  grades: publicProcedure.input(z.object({ department: z.string(), number: z.string() })).query(async ({ input }) => {
    const r = fetch(
      `${process.env.PUBLIC_API_URL}grades/raw?department=${encodeURIComponent(input.department)}&courseNumber=${input.number}`,
    );

    return r.then((response) => response.json()).then((data) => data.data as GradesRaw);
  }),
});

export default coursesRouter;
