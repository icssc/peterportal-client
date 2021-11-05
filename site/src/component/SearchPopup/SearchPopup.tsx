import React, { FC } from 'react';
import './SearchPopup.scss'
import searching from '../../asset/searching.gif';
import GradeDist from '../GradeDist/GradeDist';
import Button from 'react-bootstrap/Button';
import Carousel from "react-multi-carousel";

import { useAppSelector } from '../../store/hooks';
import { selectCourse, selectProfessor } from '../../store/slices/popupSlice';
import { CourseData, ProfessorData, SearchType } from '../../types/types';

interface InfoData {
    title: string;
    content: string | React.FunctionComponent;
}

interface ScoreData {
    name: string;
    score: number;
}

interface SearchPopupProps {
    searchType: SearchType;
    id: string;
    title: string;
    infos: InfoData[];
    scores: ScoreData[];
    course?: CourseData;
    professor?: ProfessorData;
}

const SearchPopup: FC<SearchPopupProps> = (props) => {
    const course = useAppSelector(selectCourse);
    const professor = useAppSelector(selectProfessor);

    let selected = false;
    if (props.searchType == 'course') {
        selected = course != null;
    }
    else if (props.searchType == 'professor') {
        selected = professor != null;
    }

    if (!selected) {
        return <div className='search-popup'>
            <div className='search-popup-missing'>
                <img src={searching} alt='searching' />
                <p>
                    Click on a {props.searchType} card to view more information!
                </p>
            </div>
        </div>
    }
    else {
        return <SearchPopupContent {...props} />
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

const SearchPopupContent: FC<SearchPopupProps> = (props) => {
    return <div>
        <div className='search-popup'>
            <div className='search-popup-header'>
                <h2 className='search-popup-id'>
                    {props.id}
                    <a href={`/${props.searchType}/${props.id}`}>
                        <Button type='button' className="search-popup-more btn btn-outline-primary">
                            More Information
                        </Button>
                    </a>
                </h2>
                <h5 className='search-popup-title'>{props.title}</h5>
            </div>
            <div>
                <div className='search-popup-infos'>
                    {
                        props.infos.map((info, i) => <div className='search-popup-info search-popup-block' key={`search-popup-info-${i}`}>
                            <h3>
                                {info.title}
                            </h3>
                            <p>
                                {info.content || `No ${info.title}`}
                            </p>
                        </div>)
                    }
                </div>

                <h2 className="search-popup-label">
                    Grade Distribution
                </h2>
                <div className="search-popup-block">
                    <GradeDist course={props.course} professor={props.professor} minify={true} />
                </div>

                <h2 className="search-popup-label">
                    {props.searchType == 'course' ? 'Current Instructors' : 'Previously Taught'}
                </h2>
                <div>
                    <Carousel responsive={responsive} renderButtonGroupOutside>
                        {props.scores.map((score, i) => <div key={`search-popup-carousel-${i}`} className='search-popup-carousel search-popup-block'>
                            <div>
                                <span className='search-popup-carousel-score'>{score.score}</span>
                                <span className='search-popup-carousel-max-score'>/ 5.0</span>
                            </div>
                            <p>{score.name}</p>
                        </div>
                        )}
                    </Carousel>
                </div>
            </div>
        </div>
    </div>
}

export default SearchPopup;