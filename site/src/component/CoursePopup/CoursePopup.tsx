import React, { FC } from 'react';
import './CoursePopup.scss'
import searching from '../../asset/searching.gif';
import GradeDist from '../GradeDist/GradeDist';
import Button from 'react-bootstrap/Button';
import Carousel from "react-multi-carousel";

import { useAppSelector } from '../../store/hooks';
import { selectCourse } from '../../store/slices/popupSlice';
import { useProfessorNames } from '../../hooks/professorNames';
import { CourseData } from '../../types/types';

const CoursePopupRoot: FC = () => {
    const course = useAppSelector(selectCourse);

    if (!course) {
        return <div className='course-popup'>
            <div className='course-popup-missing'>
                <img src={searching} alt='searching' />
                <p>
                    Click on a course card to view course information!
                </p>
            </div>
        </div>
    }
    else {
        return <CoursePopup course={course} />
    }
}

const responsive = {
    desktop: {
        breakpoint: { max: 3000, min: 1024 },
        items: 3,
        paritialVisibilityGutter: 60
    },
    tablet: {
        breakpoint: { max: 1024, min: 464 },
        items: 2,
        paritialVisibilityGutter: 50
    },
    mobile: {
        breakpoint: { max: 464, min: 0 },
        items: 1,
        paritialVisibilityGutter: 30
    }
};

interface CoursePopupPros {
    course: CourseData;
}

const CoursePopup: FC<CoursePopupPros> = (props) => {
    const { loading, error, professorNames } = useProfessorNames(props.course.id);

    return <div>
        <div className='course-popup'>
            <div className='course-popup-header'>
                <h2 className='course-popup-id'>
                    {props.course.id}
                    <a href={'/course/' + props.course.id}>
                        <Button type='button' className="course-popup-more btn btn-outline-primary">
                            More Information
                        </Button>
                    </a>
                </h2>
                <h5 className='course-popup-title'>{props.course.title}</h5>
            </div>
            <div>
                <div className='course-popup-infos'>
                    <div className='course-popup-info course-popup-block'>
                        <h3>
                            Prerequisite
                        </h3>
                        <p>
                            {props.course.prerequisite_text || 'No Prerequisites'}
                        </p>
                    </div>
                    <div className='course-popup-info course-popup-block'>
                        <h3>
                            Restrictions
                        </h3>
                        <p>
                            {props.course.restriction || 'No Restrictions'}
                        </p>
                    </div>
                </div>

                <h2 className="course-popup-label">
                    Grade Distribution
                </h2>
                <div className="course-popup-block">
                    <GradeDist course={props.course} minify={true} />
                </div>

                <h2 className="course-popup-label">
                    Current Instructors
                </h2>
                <div className='course-popup-professors'>
                    <Carousel responsive={responsive} renderButtonGroupOutside>
                        {professorNames.map((prof) => <div key={`course-popup-professor-${prof}`} className='course-popup-professor course-popup-block'>
                            <div>
                                <span className='course-popup-professor-score'>5.0</span>
                                <span className='course-popup-professor-max-score'>/ 5.0</span>
                            </div>
                            <p>{prof.name}</p>
                        </div>
                        )}
                    </Carousel>
                </div>
            </div>
        </div>
    </div>
}

export default CoursePopupRoot;