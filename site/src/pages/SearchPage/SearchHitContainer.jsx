import React from 'react'
import ProfessorHitItem from "./ProfessorHitItem.jsx"
import CourseHitItem from "./CourseHitItem.jsx"
import { Icon, Label } from "semantic-ui-react"
import { Pagination, Hits, InitialLoader, SelectedFilters } from "searchkit"
import './SearchHitContainer.scss'

// Array of highlighted fields. Any highlight matches will be render
// as <highlight> html element
const highlightFieldValues = {
    "courses": [
        "name",
        "id_department",
        "id_number",
        "description",
    ],
    "professors": [
        "name",
        "title",
        "department",
        "courseHistory"
    ]
}

// Custom rendering of InitialLoader component
const InitialLoaderComponent = props => <div style={{fontSize: "100pt"}}>Fetching course data...</div>;

// Custom rendering of SelectedFilter component
const SelectedFilter = (props) => (
    <Label color='blue' onClick={props.removeFilter}     as='a'>
        <span style={{marginRight: "0.8rem"}}>{props.labelValue}</span>
        <Icon name='close'/>
    </Label>
)

function SearchHitContainer(props) {
    return(
        <div className="search-hit-container">
            <div style={{ minHeight: "100vh", marginLeft: "34px" }}>
                <SelectedFilters itemComponent={SelectedFilter} />
                <Hits
                    itemComponent={props.query === "courses" ? CourseHitItem : ProfessorHitItem}
                    hitsPerPage={20}
                    highlightFields={highlightFieldValues[props.query]}
                    customHighlight={{
                        pre_tags: ["<highlight>"],
                        post_tags: ["</highlight>"],
                    }}
                />
            </div>
            <InitialLoader component={InitialLoaderComponent} />
            <Pagination showNumbers={true} />
        </div>
    )
}

export default SearchHitContainer