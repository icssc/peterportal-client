import React, { FC, useState, useEffect } from 'react'
import './ProfessorPage.scss';
import { RouteComponentProps } from 'react-router-dom';
import LoadingPage from '../LoadingPage';
import Twemoji from 'react-twemoji';
import { Divider } from 'semantic-ui-react';
import axios from 'axios';
import Schedule from '../../component/Schedule/Schedule';
import Review from '../../component/Review/Review';
import GradeDist from '../../component/GradeDist/GradeDist';
import SideInfo from '../../component/SideInfo/SideInfo';
import Error from '../../component/Error/Error';

import { setProfessor } from '../../store/slices/popupSlice';
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { ProfessorGQLData } from '../../types/types';
import { searchAPIResult } from '../../helpers/util';

const ProfessorPage: FC<RouteComponentProps<{ id: string }>> = (props) => {
    const dispatch = useAppDispatch();
    const professorGQLData = useAppSelector(state => state.popup.professor);
    const [error, setError] = useState('');

    useEffect(() => {
        // make a gql query if directly landed on this page
        if (professorGQLData == null || professorGQLData.ucinetid != props.match.params.id) {
            searchAPIResult('professor', props.match.params.id)
                .then(professor => {
                    if (professor) {
                        dispatch(setProfessor(professor as ProfessorGQLData))
                    }
                    else {
                        setError(`Professor ${props.match.params.id} does not exist!`);
                    }
                })
        }
    }, [])

    // if professor does not exists
    if (error) {
        return <Error message={error} />
    }
    // loading results
    else if (!professorGQLData) {
        return <LoadingPage />;
    }
    else {
        return (
            <Twemoji options={{ className: 'twemoji' }}>
                <div className='professor-page'>
                    <div>
                        <SideInfo searchType='professor' name={professorGQLData.name}
                            title={professorGQLData.title} school={professorGQLData.schools[0]} description={professorGQLData.department}
                            tags={[professorGQLData.ucinetid, professorGQLData.shortenedName]} professor={professorGQLData} />
                    </div>
                    <article className='professor-page-body'>
                        <div className='professor-page-section'>
                            <div>
                                <h2>🗓️ Schedule of Classes</h2>
                            </div>
                            <Divider />
                            <Schedule professorID={professorGQLData.shortenedName} />
                        </div>

                        <div className='professor-page-section'>
                            <div>
                                <h2>📊 Grade Distribution</h2>
                            </div>
                            <Divider />
                            <GradeDist professor={professorGQLData} />
                        </div>

                        <div className='professor-page-section'>
                            <div>
                                <h2>💬 Reviews</h2>
                            </div>
                            <Divider />
                            <Review professor={professorGQLData} />
                        </div>
                    </article>
                </div>
            </Twemoji>
        )
    }
}

export default ProfessorPage