import React, { useState, useEffect, Component, FC } from 'react';
import './SearchHitContainer.scss';

import { useAppSelector } from '../../store/hooks';

import { SearchIndex, CourseData, ProfessorData } from '../../types/types';

interface SearchHitContainerProps {
    index: SearchIndex;
    CourseHitItem: FC<CourseData & { index: number }>;
    ProfessorHitItem?: FC<ProfessorData & { index: number }>;
}

const SearchHitContainer: FC<SearchHitContainerProps> = ({ index, CourseHitItem, ProfessorHitItem }) => {
    const courseResults = useAppSelector(state => state.search.courses.results);
    const professorResults = useAppSelector(state => state.search.professors.results);

    if (index == 'professors' && !ProfessorHitItem) {
        throw 'Professor Component not provided';
    }

    return <div className='search-hit-container'>
        {
            index == 'courses' && <>
                {
                    courseResults.map((course, i) => {
                        return <CourseHitItem key={`course-hit-item-${i}`} index={i} {...(course as CourseData)} />
                    })
                }
            </>
        }
        {
            (index == 'professors' && ProfessorHitItem) && <>
                {
                    professorResults.map((professor, i) => {
                        return <ProfessorHitItem key={`professor-hit-item-${i}`} index={i} {...(professor as ProfessorData)} />
                    })
                }
            </>
        }
    </div>
}

export default SearchHitContainer;