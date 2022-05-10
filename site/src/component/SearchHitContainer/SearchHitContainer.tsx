import React, { useState, useEffect, Component, FC } from 'react';
import './SearchHitContainer.scss';
import CourseHitItem from '../CourseHitItem/CourseHitItem';
import ProfessorHitItem from '../ProfessorHitItem/ProfessorHitItem';

import { useAppSelector } from '../../store/hooks';

import { SearchIndex, CourseData, ProfessorData } from '../../types/types';

interface SearchHitContainerProps {
    index: SearchIndex;
}

const SearchHitContainer: FC<SearchHitContainerProps> = ({ index }) => {
    const courseResults = useAppSelector(state => state.search.courses.results);
    const professorResults = useAppSelector(state => state.search.professors.results);

    return <div className='search-hit-container'>
        {
            index == 'courses' && <>
                {
                    courseResults.map((course, i) => {
                        return <CourseHitItem key={`course-hit-item-${i}`} {...(course as CourseData)} />
                    })
                }
            </>
        }
        {
            index == 'professors' && <>
                {
                    professorResults.map((professor, i) => {
                        return <ProfessorHitItem key={`professor-hit-item-${i}`} {...(professor as ProfessorData)} />
                    })
                }
            </>
        }
    </div>
}

export default SearchHitContainer;