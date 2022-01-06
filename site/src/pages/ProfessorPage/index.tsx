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

import { ProfessorData, CourseData, ReviewData } from '../../types/types';

const ProfessorPage: FC<RouteComponentProps<{ id: string }>> = (props) => {
    const [profData, setProfData] = useState<ProfessorData>(null!);
    const [profWebsoc, setProfWebsoc] = useState('');
    const fetchDataFromApi = async () => {
        const apiResponse = await axios.get<ProfessorData>('/professors/api/' + props.match.params.id);
        setProfWebsoc(apiResponse.data.shortened_name);
        setProfData(apiResponse.data);
    }

    useEffect(() => {
        fetchDataFromApi();
    }, []);

    if (!profData) {
        return <LoadingPage />;
    }
    else if (profData.hasOwnProperty('error')) {
        console.log(profData)
        return <div>
            Professor Does Not Exist!
        </div>
    } else {
        return (
            <Twemoji options={{ className: 'twemoji' }}>
                <div className='professor-page'>
                    <div>
                        <SideInfo searchType='professor' name={profData.name}
                            title={profData.title} school={profData.schools[0]} description={profData.department}
                            tags={[profData.ucinetid, profData.shortened_name]} professor={profData} />
                    </div>
                    <article className='professor-page-body'>
                        <div className='professor-page-section'>
                            <div>
                                <h2>🗓️ Schedule of Classes</h2>
                            </div>
                            <Divider />
                            <Schedule professorID={profWebsoc} />
                        </div>

                        <div className='professor-page-section'>
                            <div>
                                <h2>📊 Grade Distribution</h2>
                            </div>
                            <Divider />
                            <GradeDist professor={profData} />
                        </div>

                        <div className='professor-page-section'>
                            <div>
                                <h2>💬 Reviews</h2>
                            </div>
                            <Divider />
                            <Review professor={profData} />
                        </div>
                    </article>
                </div>
            </Twemoji>
        )
    }
}

export default ProfessorPage