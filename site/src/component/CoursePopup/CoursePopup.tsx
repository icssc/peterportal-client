import React, { FC } from 'react';
import './CoursePopup.scss'
import GradeDist from '../GradeDist/GradeDist';
import { useAppSelector } from '../../store/hooks';
import { selectCourse } from '../../store/slices/popupSlice';
import { useProfessorNames } from '../../hooks/professorNames';
import { CourseData } from '../../types/types';

const CoursePopupRoot: FC = () => {
    const course = useAppSelector(selectCourse);

    if (!course) {
        return <div className='course-popup-missing'>
            Click on the course card to view course information!
        </div>
    }
    else {
        return <CoursePopup course={course} />
    }
}

interface CoursePopupPros {
    course: CourseData;
}

const CoursePopup: FC<CoursePopupPros> = (props) => {
    const { loading, error, professorNames } = useProfessorNames(props.course.id);

    return <div>
        <div className='course-popup'>
            <div style={{ marginLeft: '1.5rem' }}>
                <h2 className='course-popup-id'>{props.course.id}</h2>
                <h5 className='course-popup-title'>{props.course.title}</h5>
                <a href={'/course/' + props.course.id}>More Information</a>
            </div>
            <div>
                <div className='course-popup-info'>
                    <p>
                        Prerequisite: {props.course.prerequisite_text}
                    </p>
                </div>
                <GradeDist course={props.course} minify={true} />
                <div>
                    <h5>Instructor History</h5>
                    {professorNames.map((prof) =>
                        <div key={`instr-hist-${prof.ucinetid}`}>
                            <span>
                                {prof.name}
                            </span>
                        </div>
                    )}
                </div>
            </div>
        </div>
    </div>
}

export default CoursePopupRoot;