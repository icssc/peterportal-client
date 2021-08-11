import React from 'react';
import { RouteComponentProps } from 'react-router-dom';
import './SearchPage.scss';
import SearchModule from './SearchModule'
import SearchSidebar from './SearchSidebar'
import SearchHitContainer from './SearchHitContainer'
import { SearchkitComponent, SearchkitManager, SearchkitProvider, SearchkitComponentProps } from 'searchkit';
import Sidebar from '../../component/SideBar/SideBar'
import CoursePopup from '../../component/CoursePopup/CoursePopup'
import ProfessorPopup from '../../component/ProfessorPopup/ProfessorPopup'

import { ElasticSearchIndex } from '../../types/types';

interface SearchPageProps extends RouteComponentProps<{ index: ElasticSearchIndex }>, SearchkitComponentProps {
}

export default class SearchPage extends SearchkitComponent<SearchPageProps, {}> {
    render() {
        // 'this.props.match.params.index' is used to determine which index to 
        // query via url location - i.e: (professor || courses)
        let searchkit = new SearchkitManager('/' + this.props.match.params.index);

        return (
            <SearchkitProvider searchkit={searchkit}>
                <>
                    <Sidebar>
                        <aside>
                            <hr style={{ margin: '15px 0', backgroundColor: 'var(--peterportal-light-gray)', height: '1px', borderWidth: '0' }} />
                            <SearchSidebar query={this.props.match.params.index} />
                        </aside>
                    </Sidebar>
                    <div style={{ display: 'flex', flexGrow: 1 }}>
                        <div style={{ overflow: 'scroll', height: '100vh', width: '55vw', overflowX: 'hidden' }}>
                            <SearchModule query={this.props.match.params.index} />
                            <SearchHitContainer query={this.props.match.params.index} />
                        </div>
                        <div style={{ flexGrow: 1 }}>
                            {this.props.match.params.index == 'courses' && <CoursePopup />}
                            {this.props.match.params.index == 'professors' && <ProfessorPopup />}
                        </div>
                    </div>
                </>
            </SearchkitProvider>
        );
    }
}

