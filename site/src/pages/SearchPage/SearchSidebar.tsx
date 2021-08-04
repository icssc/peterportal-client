import React, { FC } from 'react';
import CourseFilter from './CourseFilter';
import ProfessorFilter from './ProfessorFilter';
import { Menu } from 'semantic-ui-react';

import { ElasticSearchIndex } from '../../types/types';

interface SearchSidebarProps {
    query: ElasticSearchIndex;
}

const SearchSidebar: FC<SearchSidebarProps> = (props) => {
    return (
        <div className='sidebar-container'>
            <Menu secondary vertical className='mode-switcher'>
                <a href='/search/courses'>
                    <Menu.Item
                        name='courses'
                        active={props.query === 'courses'}
                    />
                </a>
                <a href='/search/professors'>
                    <Menu.Item
                        name='professors'
                        active={props.query === 'professors'}
                    />
                </a>
            </Menu>

            {props.query === 'courses' && <CourseFilter />}
            {props.query === 'professors' && <ProfessorFilter />}
        </div>
    )
}

export default SearchSidebar;