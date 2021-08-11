import React, { FC } from 'react';
import './ProfessorPopup.scss'
import GradeDist from '../GradeDist/GradeDist';
import { useAppSelector } from '../../store/hooks';
import { selectProfessor } from '../../store/slices/popupSlice';
import { ProfessorData } from '../../types/types';

const ProfessorPopupRoot: FC = () => {
    const professor = useAppSelector(selectProfessor);

    if (!professor) {
        return <div className='professor-popup-missing'>
            Click on the professor card to view professor information!
        </div>
    }
    else {
        return <ProfessorPopup professor={professor} />
    }
}

interface ProfessorPopupPros {
    professor: ProfessorData;
}

const ProfessorPopup: FC<ProfessorPopupPros> = (props) => {
    return <div>
        <div className='professor-popup'>
            <div style={{ marginLeft: '1.5rem' }}>
                <h2 className='professor-popup-id'>{props.professor.ucinetid}</h2>
                <h5 className='professor-popup-title'>{props.professor.title}</h5>
                <a href={'/professor/' + props.professor.ucinetid}>More Information</a>
            </div>
            <div>
                <GradeDist professor={props.professor} />
                <div>
                    <h5>Courses Taught</h5>
                    {props.professor.course_history.map((course) =>
                        <div key={`instr-hist-${course}`}>
                            <span>
                                {course}
                            </span>
                        </div>
                    )}
                </div>
            </div>
        </div>
    </div>
}

export default ProfessorPopupRoot;