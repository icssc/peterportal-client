import React, { FC } from 'react'
import { Icon, Label } from 'semantic-ui-react'
import { HitsStats, SearchBox, SelectedFilters, FilterItemProps, HitsStatsDisplayProps } from 'searchkit';
import './SearchModule.scss';
import { Filter } from 'react-bootstrap-icons';
import { Button } from 'react-bootstrap';
import { setFilterStatus } from '../../store/slices/uiSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';

import { ElasticSearchIndex } from '../../types/types';

// An array of elasticsearch fields to search within. Can specify boosting on particular fields.
// Modify this will affect search result.
const queryFieldValues = {
    'courses': [
        'department^10',
        'number^10',
        'description',
        'department_alias^10',
        'title^3',
    ],
    'professors': [
        'name^10',
        'ucinetid^10',
        'title^3',
        'course_history',
        'department^3',
    ]
}

// Custom rendering of customHitStats component
const customHitStats = (props: HitsStatsDisplayProps) => (
    <div>
        <p className={'hit-stats'}>{props.hitsCount} results found in {props.timeTaken}ms</p>
    </div>
)

// Custom rendering of SelectedFilter component
const SelectedFilter = (props: FilterItemProps) => (
    <Label color='blue' onClick={(event, data) => { props.removeFilter(event, data) }} as='a'>
        <span style={{ marginRight: '0.8rem' }}>{props.labelValue}</span>
        <Icon name='close' />
    </Label>
)

interface SearchModuleProps {
    query: ElasticSearchIndex;
}

const SearchModule: FC<SearchModuleProps> = (props) => {
    const dispatch = useAppDispatch();
    const filterOpen = useAppSelector(state => state.ui.filterOpen);

    const toggleFilter = () => {
        dispatch(setFilterStatus(!filterOpen));
    }

    return (
        <div>
            <section className='search-module'>
                <div className='search-bar'>
                    <SearchBox
                        autofocus={true}
                        searchOnChange={true}
                        queryFields={queryFieldValues[props.query]}
                        searchThrottleTime={300}
                        placeholder={props.query === 'courses' ? 'Course number, title and description' : 'Professor name, title, and department'}
                    />
                    <Button className='search-filter-button' onClick={toggleFilter}>
                        <Filter size={25} />
                        Filter
                    </Button>
                </div>
                {/* <HitsStats component={customHitStats} /> */}
                <SelectedFilters itemComponent={SelectedFilter} />
            </section >
        </div >
    );
}

export default SearchModule;