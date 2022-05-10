import React, { FC } from 'react'
import { Icon, Label } from 'semantic-ui-react'
import { HitsStats, SearchBox, SelectedFilters, FilterItemProps, HitsStatsDisplayProps } from 'searchkit';
import './SearchModule.scss';

import { SearchIndex } from '../../types/types';

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
    query: SearchIndex;
}

const SearchModule: FC<SearchModuleProps> = (props) => {
    return (
        <section className='roadmaps-search-module'>
            {/* <h1 className='search-title'>Search by </h1> */}
            <SearchBox
                autofocus={true}
                searchOnChange={true}
                queryFields={queryFieldValues[props.query]}
                searchThrottleTime={300}
                placeholder={props.query === 'courses' ? 'Course number, title and description' : 'Professor name, title, and department'}
            />
            <HitsStats component={customHitStats} />
            <SelectedFilters itemComponent={SelectedFilter} />
        </section>
    );
}

export default SearchModule;