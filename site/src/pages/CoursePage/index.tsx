import React, { FC, useState, useEffect } from 'react'
import { RouteComponentProps } from 'react-router-dom';
import LoadingPage from '../LoadingPage';
import Twemoji from 'react-twemoji';
import { Divider } from 'semantic-ui-react';
import axios from 'axios';

import GradeDist from '../../component/GradeDist/GradeDist';
import PrereqTree from '../../component/PrereqTree/PrereqTree';
import Schedule from '../../component/Schedule/Schedule';
import Review from '../../component/Review/Review';
import SideInfo from '../../component/SideInfo/SideInfo';
import Error from '../../component/Error/Error';
import { useCourseGQL } from '../../hooks/courseData';

import { getCourseTags } from '../../helpers/util';
import { CourseData } from '../../types/types';
import './CoursePage.scss';

const CoursePage: FC<RouteComponentProps<{ id: string }>> = (props) => {
    const [courseData, setCourseData] = useState<CourseData>(null!);
    const { loading, error, course: courseGQLData } = useCourseGQL(props.match.params.id);

    const fetchDataFromApi = async () => {
        console.log('Viewing Course', props.match.params.id);
        const apiResponse = await axios.get('/courses/api', {
            params: {
                courseID: props.match.params.id
            }
        });
        setCourseData(apiResponse.data);
    }

    useEffect(() => {
        fetchDataFromApi();
    }, []);

    if (!courseData) {
        return <LoadingPage />;
    }
    else if (courseData.hasOwnProperty('error')) {
        return <Error message='Course Does Not Exist!' />
    } else {
        return (
            <Twemoji options={{ className: 'twemoji' }}>
                <div className='course-page'>
                    <div>
                        <SideInfo searchType='course' name={courseData.department + ' ' + courseData.number}
                            title={courseData.title} school={courseData.school} description={courseData.description}
                            tags={getCourseTags(courseData)} course={courseGQLData} />
                    </div>
                    <div className='course-page-body'>
                        <div className='course-page-section'>
                            <div>
                                <h2>🌲 Prerequisite Tree</h2>
                            </div>
                            <Divider />
                            <PrereqTree {...courseGQLData} />
                        </div>

                        <div className='course-page-section'>
                            <div>
                                <h2>🗓️ Schedule of Classes</h2>
                            </div>
                            <Divider />
                            <Schedule courseID={courseGQLData.department + ' ' + courseGQLData.number} />
                        </div>

                        <div className='course-page-section'>
                            <div>
                                <h2>📊 Grade Distribution</h2>
                            </div>
                            <Divider />
                            <GradeDist course={courseData} />
                        </div>

                        <div className='course-page-section'>
                            <div>
                                <h2>💬 Reviews</h2>
                            </div>
                            <Divider />
                            <Review course={courseGQLData} />
                        </div>
                    </div>
                </div>
            </Twemoji>
        )
    }
}

export default CoursePage;