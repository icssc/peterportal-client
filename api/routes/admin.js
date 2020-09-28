var express = require('express');
var router = express.Router();
var { executeQuery, escape } = require('../config/database.js');
var { clearCacheByID, clearCacheAll, statistics } = require('../api/v1/cache');
var { getTerms } = require("../api/v1/schedule")
var fetch = require("node-fetch");
const WebSocAPI = require("websoc-api");
const dotenv = require('dotenv');
dotenv.config();

const REVIEW_STATUSES = ["published", "removed", "unverified"];
const TERM_SEASONS = ['Winter', 'Spring', 'Summer1', 'Summer10wk', 'Summer2', 'Fall']

router.use('*', function (req, res, next) {
    if (req.session.passport && req.session.passport.admin) {
        console.log("Permission Granted!");
        next();
    }
    else {
        if (process.env.NODE_ENV == "development") {
            next();
        }
        else {
            res.render('admin_home');
            console.log("Access Restricted")
        }
    }
});

// get reviews of all statuses
router.get("/reviews", function (req, res) {
    // couters to tell if all asynchronous functions are done
    count = 0
    finish = REVIEW_STATUSES.length;
    data = {};
    REVIEW_STATUSES.forEach((status) => {
        let sql = `SELECT * FROM reviews AS r 
        WHERE r.pub_status = "${status}" 
        ORDER BY r.submitted_at DESC`;
        // execute asynchronously
        executeQuery(sql, function (results) {
            count += 1;
            data[status] = results
            if (count == finish) {
                res.json(data);
            }
        });
    })
});

// set the status of a review
// reviewID: the review to change the status of
// status: the new status to change it to
router.put("/reviews/setStatus", function (req, res) {
    if (!REVIEW_STATUSES.includes(req.body.status)) {
        res.status(400).send(`Invalid Status! Given: ${req.body.status}. Accepted: ${REVIEW_STATUSES}`);
        return;
    }
    let sql = `UPDATE reviews SET pub_status = ${escape(req.body.status)} WHERE id = ${req.body.reviewID}`
    executeQuery(sql, function (results) {
        res.json(results);
    });
});


//get all flagged reviews that need to be checked
router.get("/flagged", function (req, res) {
    let sql = `SELECT * 
        FROM flagged as f, reviews as r
        WHERE f.review_id = r.id AND flag_status='pending' 
        ORDER BY reported_at ASC`;

    executeQuery(sql, function (results) {
        res.json(results);
    });
});

//update a flagged review
//status: 
router.put("/flagged/update", function (req, res) {
    let sql = `UPDATE flagged
        SET flag_status = '${req.body.status}', fulfilled_at='${req.body.date}', fulfill_by='${req.body.username}'
        WHERE flag_id = ${req.body.flagID};`

    executeQuery(sql, function (results) {
        res.json(results);
    });
});

// docID: the cache name
// clears the cache
router.get("/clearCache", function(req, res){
    let docID = req.query.docID;
    if(docID){
        res.send(clearCacheByID(docID));
    }
    else {
        res.send(clearCacheAll());
    }
})

// reports hits and misses for the cache
router.get("/cache", function (req, res) {
    res.json(statistics());
})

// pastYears: how many years to go in the past
// assign terms for the past years
router.get("/assignTerms", function (req, res){
    let pastYears = req.query.pastYears;
    terms = getTerms(pastYears);
    count = 0;
    finish = terms.length;
    results = {}
    terms.forEach( term => {
        assignTerm(term, (status, text) => {
            results[term] = [status, text];
            if(++count == finish)
                res.json(results);
        });
    });
});

// term: the year + season (eg. "2020 Fall")
// adds to the term field for each course in elasticsearch index
router.get("/assignTerm", function (req, res) {
    let term = req.query.term;
    if (!term) {
        res.status(400).send("Please provide a term!");
        return;
    }
    if (!term.match(/^[0-9]{4} [^ ]*$/)) {
        res.status(400).send("Bad format!");
        return;
    }
    let split = term.split(" ")
    let season = split[1];
    if (!TERM_SEASONS.includes(season)) {
        res.status(400).send(`Must provide a valid season! Given: ${season}. Expected: ${TERM_SEASONS}`);
        return;
    }
    assignTerm(term, (status, text) => {
        res.status(status).send(text);
    });
});

function assignTerm(term, callback) {
    getAllCourses((err, courses) => {
        if (err) console.log(err);
        let updateJSON = ""
        let hits = 0;
        // maps a department to a list of courses
        let departments = new Set()
        // maps a course to its data
        let courseToData = {}
        courses.forEach((courseData) => {
            departments.add(courseData.department)
            courseToData[courseData.courseID] = courseData
            // if no terms list
            if (!courseData["terms"]) {
                console.log(courseData)
                ++hits;
                courseData["terms"] = []
                updateJSON += `{ "update" : {"_id" : "${courseData.courseID}", "_index" : "courses"}}\n{ "doc" : {"terms" : ${JSON.stringify(courseData.terms)}}}\n`
            }
        });
        let count = 0
        let finish = departments.size
        // search up each department on websoc
        departments.forEach(department => {
            // get available courses for a department
            getAvailableCourses(department, term, (availableCourses) => {
                // go through each available course
                availableCourses.forEach(availableCourse => {
                    // get the associated data
                    let courseData = courseToData[availableCourse]
                    // if this course is indexed
                    if (courseData) {
                        ++hits;
                        // add the term if it is not already included
                        if (!courseData["terms"].includes(term)) {
                            courseData["terms"].push(term);
                            // add to the bulk json
                            updateJSON += `{ "update" : {"_id" : "${courseData.courseID}", "_index" : "courses"}}\n{ "doc" : {"terms" : ${JSON.stringify(courseData.terms)}}}\n`
                        }
                    }
                });
                // finished all departments
                if (++count == finish) {
                    // if theres anything to update
                    if (updateJSON) {
                        // bulk update
                        fetch(`${process.env.ELASTIC_ENDPOINT_URL}/_bulk`, {
                            method: 'POST',
                            headers: {
                                'Content-Type': 'application/json',
                            },
                            body: updateJSON
                        }).then(r => r.json())
                            .then(j => callback(200, `Successfully added ${term} to ${j.items.length} courses!`))
                    }
                    else {
                        if (hits) {
                            callback(400, `${hits} courses were found, but they were already assigned ${term}!`);
                        }
                        else {
                            callback(400, "No hits were found! Invalid term!");
                        }
                    }
                }
            });
        });
    });
}


// gets all existing courses from elastic search
function getAllCourses(callback) {
    // fetch data from elastic search
    fetch(`${process.env.ELASTIC_ENDPOINT_URL}/courses/_search`, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
        },
        body:
            JSON.stringify({
                "_source": ["id", "terms", "id_department"],
                "query": {
                    "match_all": {}
                },
                "size": 10000
            })
    }).then((response) => response.json())
        .then((result) => {
            // process and return data
            var array_result = []
            result.hits.hits.forEach((e) => {
                array_result.push({ courseID: e._source.id.replace(/ /g, ''), terms: e._source.terms, department: e._source.id_department })
            })
            callback(null, array_result);
        })
        .catch((err) => callback(err, null));
}

// department: the department to search (eg I&C SCI)
// term: the term to search (eg 2020 Fall)
function getAvailableCourses(department, term, callback) {
    const opts = {
        term: term,
        department: department
    };
    WebSocAPI.callWebSocAPI(opts)
        .then(json => {
            availableCourses = []
            for (schoolKey in json['schools']) {
                school = json['schools'][schoolKey];
                for (departmentKey in school['departments']) {
                    department = school['departments'][departmentKey];
                    for (courseKey in department['courses']) {
                        course = department['courses'][courseKey];
                        courseID = course['deptCode'].replace(/ /g, '') + (course['courseNumber'])
                        availableCourses.push(courseID);
                    }
                }
            }
            callback(availableCourses);
        })
}

module.exports = router;