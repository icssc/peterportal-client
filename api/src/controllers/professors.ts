/**
 @module ProfessorsRoute
*/

import express, { Request } from 'express';
import fetch from 'node-fetch';
import { GenericObject } from '../types/types';
import professorDummy from '../dummy/professor.json';
import { getProfessorQuery } from '../helpers/gql';

var router = express.Router();

/**
 * PPAPI proxy for professor data 
 */
router.get('/api', function (req: Request<{}, {}, {}, { ucinetid: string }>, res) {
  let r = fetch(process.env.PUBLIC_API_URL + 'instructors/' + req.query.ucinetid);

  r.then((response) => response.json())
    .then((data) => res.send(data))
});

/**
 * PPAPI proxy for professor data 
 */
router.post('/api/batch', (req: Request<{}, {}, { professors: string[] }>, res) => {
  if (req.body.professors.length == 0) {
    res.json({});
  }
  else {
    let r = fetch(process.env.PUBLIC_API_GRAPHQL_URL, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        query: getProfessorQuery(req.body.professors)
      })
    });

    r.then((response) => response.json())
      .then((data) => res.json(data.data))
  }
});

/**
 * PPAPI proxy for grade distribution
 */
router.get('/api/grades/:name', function (req, res, next) {
  let r = fetch(process.env.PUBLIC_API_URL + 'grades/raw?instructor=' + encodeURIComponent(req.params.name));

  let status = 200;

  r.then((response) => {
    status = response.status;
    return response.json();
  }).then((data) => res.status(status).send(data))
});

export default router;