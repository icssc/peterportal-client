import React, { FC } from 'react';
import CourseFilter from './CourseFilter';
import ProfessorFilter from './ProfessorFilter';

import { ElasticSearchIndex } from '../../types/types';

interface SearchFilterProps {
    query: ElasticSearchIndex;
}

const SearchFilter: FC<SearchFilterProps> = (props) => {
    return (
        <div className='filter-container'>
            {props.query === 'courses' && <CourseFilter />}
            {props.query === 'professors' && <ProfessorFilter />}
        </div>
    )
}

export default SearchFilter;