/**
 @module ProfessorsRoute
*/

import express, { Request } from 'express';
import { getProfessorQuery } from '../helpers/gql';

const router = express.Router();

/**
 * PPAPI proxy for professor data
 */
router.get('/api', function (req: Request<never, unknown, never, { ucinetid: string }>, res) {
  const r = fetch(process.env.PUBLIC_API_URL + 'instructors/' + req.query.ucinetid);

  r.then((response) => response.json()).then((data) => res.send(data));
});

/**
 * PPAPI proxy for professor data
 */
router.post('/api/batch', (req: Request<never, unknown, { professors: string[] }, never>, res) => {
  if (req.body.professors.length == 0) {
    res.json({});
  } else {
    const r = fetch(process.env.PUBLIC_API_GRAPHQL_URL, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        query: getProfessorQuery(req.body.professors),
      }),
    });

    r.then((response) => response.json()).then((data) => res.json(data.data));
  }
});

/**
 * PPAPI proxy for grade distribution
 */
router.get('/api/grades/:name', function (req, res) {
  const r = fetch(process.env.PUBLIC_API_URL + 'grades/raw?instructor=' + encodeURIComponent(req.params.name));

  let status = 200;

  r.then((response) => {
    status = response.status;
    return response.json();
  }).then((data) => res.status(status).send(data.payload));
});

export default router;
