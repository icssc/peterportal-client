/**
 @module CoursesRoute
*/

import express, { Request } from 'express';
import { getCourseQuery } from '../helpers/gql';
const router = express.Router();

/**
 * PPAPI proxy for course data
 */
router.get('/api', (req: Request<never, unknown, never, { courseID: string }, never>, res) => {
  const r = fetch(process.env.PUBLIC_API_URL + 'courses/' + encodeURIComponent(req.query.courseID), {
    headers: {
      'Content-Type': 'application/json',
      Accept: 'application/json',
    },
  });
  console.log(req.query.courseID);

  r.then((response) => response.json()).then((data) => res.send(data.payload));
});

/**
 * PPAPI proxy for course data
 */
router.post('/api/batch', (req: Request<never, unknown, { courses: string[] }, never>, res) => {
  if (req.body.courses.length == 0) {
    res.json({});
  } else {
    const r = fetch(process.env.PUBLIC_API_GRAPHQL_URL, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        query: getCourseQuery(req.body.courses),
      }),
    });

    r.then((response) => response.json()).then((data) =>
      res.json(
        Object.fromEntries(
          Object.values(data.data)
            .filter((x) => x !== null)
            .map((x) => [(x as { id: string }).id, x]),
        ),
      ),
    );
  }
});

/**
 * PPAPI proxy for grade distribution
 */
router.get('/api/grades', (req: Request<never, unknown, never, { department: string; number: string }>, res) => {
  const r = fetch(
    process.env.PUBLIC_API_URL +
      'grades/raw?department=' +
      encodeURIComponent(req.query.department) +
      '&courseNumber=' +
      req.query.number,
  );

  r.then((response) => response.json()).then((data) => {
    res.send(data.payload);
  });
});

export default router;
