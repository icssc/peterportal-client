import React from 'react'
import ProfessorHitItem from './ProfessorHitItem.jsx'
import CourseHitItem from './CourseHitItem.jsx'
import { Pagination, Hits, InitialLoader, SelectedFilters } from 'searchkit'
import './SearchHitContainer.scss'

// Array of highlighted fields. Any highlight matches will be render
// as <highlight> html element
const highlightFieldValues = {
    'courses': [
        'name',
        'department',
        'number',
        'description',
    ],
    'professors': [
        'name',
        'title',
        'department',
        'course_history'
    ]
}

// Custom rendering of InitialLoader component
const InitialLoaderComponent = props => <div style={{fontSize: '12pt'}}>Fetching course data...</div>;

export default function SearchHitContainer(props) {
    return(
        <article className='search-hit-container'>
            <div style={{ minHeight: '100vh', marginLeft: '34px' }}>
                <Hits
                    itemComponent={props.query === 'courses' ? CourseHitItem : ProfessorHitItem}
                    hitsPerPage={20}
                    highlightFields={highlightFieldValues[props.query]}
                    customHighlight={{
                        pre_tags: ['<highlight>'],
                        post_tags: ['</highlight>'],
                    }}
                />
            </div>
            <InitialLoader component={InitialLoaderComponent} />
            <Pagination showNumbers={true} />
        </article>
    )
}
