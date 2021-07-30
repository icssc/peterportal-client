import React from 'react';
import './SearchPage.scss';
import SearchModule from './SearchModule.jsx'
import SearchSidebar from './SearchSidebar.jsx'
import SearchHitContainer from './SearchHitContainer.jsx'
import {SearchkitComponent, SearchkitManager, SearchkitProvider} from 'searchkit';

import Sidebar from '../../component/SideBar/SideBar'


export default class SearchPage extends SearchkitComponent {
    render() {
        // 'this.props.match.params.index' is used to determine which index to 
        // query via url location - i.e: (professor || courses)
        let searchkit = new SearchkitManager('/' + this.props.match.params.index);

        return (
            <SearchkitProvider searchkit={searchkit}>
                <Sidebar>
                    <aside>
                        <hr style={{ margin: '15px 0', backgroundColor: 'var(--peterportal-light-gray)', height: '1px', borderWidth: '0' }}/>
                        <SearchSidebar query={this.props.match.params.index}/>
                    </aside>
                </Sidebar>
                <div>
                    <SearchModule query={this.props.match.params.index}/>
                    <SearchHitContainer query={this.props.match.params.index}/>
                </div>
            </SearchkitProvider>
        );
    }

}

