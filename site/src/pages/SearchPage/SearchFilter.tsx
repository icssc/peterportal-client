import React, { FC } from 'react';
import CourseFilter from './CourseFilter';
import ProfessorFilter from './ProfessorFilter';
import './SearchFilter.scss';
import { XCircle } from 'react-bootstrap-icons';
import { useAppDispatch } from '../../store/hooks';
import { setFilterStatus } from '../../store/slices/uiSlice';

import { ElasticSearchIndex } from '../../types/types';

interface SearchFilterProps {
    query: ElasticSearchIndex;
}

const SearchFilter: FC<SearchFilterProps> = (props) => {
    const dispatch = useAppDispatch();

    return (
        <div className='search-filter'>
            <div className='filter-header'>
                <h4>Search Filter</h4>
                <XCircle className='filter-close' size={30} onClick={() => dispatch(setFilterStatus(false))} />
            </div>
            {props.query === 'courses' && <CourseFilter />}
            {props.query === 'professors' && <ProfessorFilter />}
        </div>
    )
}

export default SearchFilter;