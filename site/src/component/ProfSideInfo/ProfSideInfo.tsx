import React, { FC } from 'react';
import { Link } from 'react-router-dom';

import { ProfessorData } from '../../types/types';

interface ProfessorSideInfoProps extends ProfessorData {
}

const ProfessorSideInfo: FC<ProfessorSideInfoProps> = (props) => {
    return (
        <div>
            <div style={{ display: 'flex', backgroundColor: '#EEEEEE', padding: '1rem' }}>
                <div>
                    💻
                </div>
                <div style={{ marginLeft: '1.5rem' }}>
                    <h2 style={{ marginBottom: '0.2rem' }}>{props.name}</h2>
                    <h5 style={{ margin: 0 }}>UCInetID: {props.ucinetid}</h5>
                </div>
            </div>

            <div style={{ padding: '1.5rem' }}>
                <p>{props.department}&nbsp;･&nbsp;{props.title}</p>

                {props.course_history && props.course_history.length != 0 &&
                    <>
                        <h5>Course History</h5>
                        {props.course_history.map((e) =>
                            <div key={`side-course-${e}`}>
                                <span>
                                    <Link to={{ pathname: `/course/${e.replace(/\s/g,'')}` }}>
                                        {e}
                                    </Link>
                                </span>
                            </div>
                        )}<br />
                    </>
                }
            </div>
        </div>
    )
}

export default ProfessorSideInfo;