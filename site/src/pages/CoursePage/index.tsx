import React, { FC, useState, useEffect } from 'react'
import { RouteComponentProps } from 'react-router-dom';
import LoadingPage from '../LoadingPage';
import Twemoji from 'react-twemoji';
import { Divider } from 'semantic-ui-react';

import GradeDist from '../../component/GradeDist/GradeDist';
import PrereqTree from '../../component/PrereqTree/PrereqTree';
import Schedule from '../../component/Schedule/Schedule';
import Review from '../../component/Review/Review';
import SideInfo from '../../component/SideInfo/SideInfo';
import Error from '../../component/Error/Error';

import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { setCourse } from '../../store/slices/popupSlice';
import { SearchType, SearchIndex, CourseGQLData } from '../../types/types';
import { getCourseTags, searchAPIResult } from '../../helpers/util';
import './CoursePage.scss';

const CoursePage: FC<RouteComponentProps<{ id: string }>> = (props) => {
    const dispatch = useAppDispatch();
    const courseGQLData = useAppSelector(state => state.popup.course);
    const [error, setError] = useState('');

    useEffect(() => {
        // make a gql query if directly landed on this page
        if (courseGQLData == null || courseGQLData.id != props.match.params.id) {
            searchAPIResult('course', props.match.params.id)
                .then(course => {
                    console.log("COURSE", course)
                    if (course) {
                        dispatch(setCourse(course as CourseGQLData))
                    }
                    else {
                        setError(`Course ${props.match.params.id} does not exist!`);
                    }
                })
        }
    }, [])

    // if course does not exists
    if (error) {
        return <Error message={error} />
    }
    // loading results
    else if (!courseGQLData) {
        return <LoadingPage />;
    }
    else {
        return (
            <Twemoji options={{ className: 'twemoji' }}>
                <div className='course-page'>
                    <div>
                        <SideInfo searchType='course' name={courseGQLData.department + ' ' + courseGQLData.courseNumber}
                            title={courseGQLData.title} school={courseGQLData.school} description={courseGQLData.description}
                            tags={getCourseTags(courseGQLData)} course={courseGQLData} />
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
                            <Schedule courseID={courseGQLData.department + ' ' + courseGQLData.courseNumber} />
                        </div>

                        <div className='course-page-section'>
                            <div>
                                <h2>📊 Grade Distribution</h2>
                            </div>
                            <Divider />
                            <GradeDist course={courseGQLData} />
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