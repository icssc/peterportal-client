import React, { useState, useEffect } from 'react'
import LoadingPage from '../LoadingPage';
import Twemoji from 'react-twemoji';
import { Divider } from "semantic-ui-react";
import axios from 'axios';

import GradeDist from '../../component/GradeDist/GradeDist';
import PrereqTree from '../../component/PrereqTree/PrereqTree';
import CourseSideInfo from '../../component/CourseSideInfo/CourseSideInfo';
import Schedule from '../../component/Schedule/Schedule';
import Review from '../../component/Review/Review';
import "./CoursePage.scss";
import {
    Grid,
  } from 'semantic-ui-react'

export default function CoursePage(props) {
    const [courseData, setCourseData] = useState(null);

    const fetchDataFromApi = async () => {
        const apiResponse = await axios.get('/courses/api/' + props.match.params.id);
        setCourseData(apiResponse.data);
    }

    useEffect(() => {
        fetchDataFromApi();
    }, []);


    if (!courseData) {
        return <LoadingPage/>;
    } else {
        return (
            <Twemoji options={{ className: 'twemoji' }}>
                <div style={{display: "flex"}}>
                    <section style={{position: "sticky", top: "4rem", height: "min-content", width: "340px", border: "1px solid #EEEEEE", borderRadius: "10px"}}>
                        <CourseSideInfo {...courseData} />
                        
                    </section>
                    <article style={{marginLeft: "4rem", width:"900px"}}>
                        <Grid.Row>
                            <h2>🌲 Prerequisite Tree</h2>
                                <Divider /> 
                            <PrereqTree {...courseData}/>
                        </Grid.Row>

                        <Grid.Row>
                            <h2>🗓️ Schedule of Classes</h2>
                                <Divider />
                            <Schedule {...courseData}/>
                        </Grid.Row>

                        <Grid.Row>
                            <h2 id="grade-dist-label">📊 Grade Distribution</h2>
                                <Divider />
                            <GradeDist {...courseData}/>
                        </Grid.Row>

                        {/* <Grid.Row>
                            <h2 id="grade-dist-label">💬 Reviews</h2>
                                <Divider />
                            <Review {...courseData}/>
                        </Grid.Row> */}
                    </article>
                </div> 
            </Twemoji>
        )

    }
}
