import React from "react";
import "./SearchPage.scss";
import SearchModule from "./SearchModule.jsx"
import SearchSidebar from "./SearchSidebar.jsx"
import SearchHitContainer from "./SearchHitContainer.jsx"
import {SearchkitComponent, SearchkitManager, SearchkitProvider} from "searchkit";


export default class SearchPage extends SearchkitComponent {
    render() {
        // 'this.props.match.params.index' is used to determine which index to 
        // query via url location - i.e: (professor || courses)
        let searchkit = new SearchkitManager("/" + this.props.match.params.index);

        return (
            <SearchkitProvider searchkit={searchkit}>
                <aside>
                    <SearchSidebar query={this.props.match.params.index}/>
                </aside>
                <div>
                    <SearchModule query={this.props.match.params.index}/>
                    <SearchHitContainer query={this.props.match.params.index}/>
                </div>
            </SearchkitProvider>
        );
    }

}

