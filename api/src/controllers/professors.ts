import express from 'express';
import fetch from 'node-fetch';

var router = express.Router();

router.post('/_search', function(req, res, next) {
  let r = fetch(process.env.PETERPORTAL_MAIN_ES + "professors/_search", 
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

router.get('/api/:ucinetid', function(req, res, next) {
  let r = fetch(process.env.PUBLIC_API_URL + "instructors/" + req.params.ucinetid, {
    headers: {
      'x-api-key': process.env.PPAPI_KEY,
    }
  });

  r.then((response) => response.json())
  .then((data) => res.send(data))
});

export default router;