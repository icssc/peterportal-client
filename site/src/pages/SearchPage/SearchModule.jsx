import React from 'react'
import { HitsStats, SearchBox} from "searchkit";
import "./SearchModule.scss";

// An array of elasticsearch fields to search within. Can specify boosting on particular fields.
// Modify this will affect search result.
const queryFieldValues = {
    "courses": [
        "id_department^10",
        "id_number^10",
        "description",
        "dept_alias^10",
        "name^3",
    ],
    "professors": [
        "name^10",
        "ucinetid^10",
        "title^3",
        "courseHistory",
        "department^3",
    ]
}

// Custom rendering of customHitStats component
const customHitStats = props => (
    <div>
        <p className={"hit-stats"}>{props.hitsCount} results found in {props.timeTaken}ms</p>
    </div>
)


function SearchModule(props) {
    return(
        <div className="search-module">
        {/* <h1 className="search-title">Search by </h1> */}
            <SearchBox
                autofocus={true}
                searchOnChange={true}
                queryFields={queryFieldValues[props.query]}
                searchThrottleTime={300}
                placeholder={props.query === "courses" ? "Course number, title and description" : "Professor name, title, and department"}
            />
            <HitsStats component={customHitStats} />
        </div>
    );
}

export default SearchModule;