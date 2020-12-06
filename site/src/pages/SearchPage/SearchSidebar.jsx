import React from 'react';
import CourseFilter from "./CourseFilter.jsx";
import ProfessorFilter from "./ProfessorFilter.jsx";
import {Menu} from "semantic-ui-react";

export default function SearchSidebar(props) {
    return(
        <div className="sidebar-container">
            <Menu secondary vertical className="mode-switcher">
                <a href="/search/courses">
                    <Menu.Item
                        name="courses"
                        icon="book"
                        active={props.query === "courses"}
                    />
                </a>
                <a href="/search/professors">
                    <Menu.Item
                        name="professors"
                        icon="graduation cap"
                        active={props.query === "professors"}
                    />
                </a>
            </Menu>

            {props.query === "courses" && <CourseFilter />}
            {props.query === "professors" && <ProfessorFilter />}

        </div>
    )
}
