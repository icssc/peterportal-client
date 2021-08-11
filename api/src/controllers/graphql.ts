/**
 @module GraphqlRouter
*/

import express from 'express';
import fetch from 'node-fetch';
var router = express.Router();

/**
 * Graphql proxy
 */
router.post('/', function (req, res, next) {
    let r = fetch(process.env.PUBLIC_API_GRAPHQL_URL,
        {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify(req.body)
        })

    r.then((response) => response.json())
        .then((data) => {
            res.send(data)
        })
        .catch(err => console.log('Error:', err))
});

export default router;