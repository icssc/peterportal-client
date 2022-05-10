/**
 @module ProfessorsRoute
*/

import express, { Request }  from 'express';
import fetch from 'node-fetch';
import { GenericObject } from '../types/types';
import professorDummy from '../dummy/professor.json';

var router = express.Router();

/**
 * Elasticsearch proxy for professor index
 */
router.post('/_search', function (req, res, next) {
  if (!process.env.PETERPORTAL_MAIN_ES) {
    res.json(professorDummy);
    return;
  }
  let r = fetch(process.env.PETERPORTAL_MAIN_ES + 'professors/_search',
    {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(req.body)
    })
  r.then((response) => response.json())
    .then((data) => res.send(data))
});

/**
 * PPAPI proxy for professor data 
 */
router.get('/api', function (req: Request<{}, {}, {}, { ucinetid: string }>, res) {
  let r = fetch(process.env.PUBLIC_API_URL + 'instructors/' + req.query.ucinetid, {
    headers: {
      'x-api-key': process.env.PPAPI_KEY,
    }
  });

  r.then((response) => response.json())
    .then((data) => res.send(data))
});

/**
 * PPAPI proxy for professor data 
 */
 router.post('/api/batch', (req: Request<{}, {}, { professors: string[] }>, res) => {
  let results: GenericObject = {};
  let count = 0;
  if (req.body.professors.length == 0) {
    res.json({});
  }
  else {
    req.body.professors.forEach(professor => {
      let r = fetch(process.env.PUBLIC_API_URL + 'instructors/' + encodeURIComponent(professor), {
        headers: {
          'x-api-key': process.env.PPAPI_KEY,
          'Content-Type': 'application/json',
          'Accept': 'application/json'
        }
      });

      r.then((response) => response.json())
        .then((data) => {
          results[professor] = data;
          count += 1;
          if (count == req.body.professors.length) {
            res.json(results);
          }
        })
    })
  }
});

/**
 * PPAPI proxy for grade distribution
 */
router.get('/api/grades/:name', function (req, res, next) {
  let r = fetch(process.env.PUBLIC_API_URL + 'grades/raw?instructor=' + encodeURIComponent(req.params.name), {
    headers: {
      'x-api-key': process.env.PPAPI_KEY
    }
  });

  r.then((response) => response.json())
    .then((data) => res.send(data))
});

export default router;