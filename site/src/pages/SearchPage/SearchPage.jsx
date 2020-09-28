import React from "react";
import "./SearchPage.scss";
import SearchModule from "./SearchModule.jsx"
import SearchSidebar from "./SearchSidebar.jsx"
import SearchHitContainer from "./SearchHitContainer.jsx"
import {SearchkitComponent, SearchkitManager, SearchkitProvider} from "searchkit";


class SearchPage extends SearchkitComponent {
    render() {
        // 'this.props.match.params.index' is used to determine which index to 
        // query via url location - i.e: (professor || courses)
        let searchkit = new SearchkitManager("/" + this.props.match.params.index);

        return (
            <SearchkitProvider searchkit={searchkit}>
                <div className="App" style={{ display: "flex", flexDirection: "row" }}>
                    <div className="search-page">
                        <div className="searchbox-container">
                            <SearchModule query={this.props.match.params.index}/>
                        </div>
                        <div className="search-content-container">
                            <SearchSidebar query={this.props.match.params.index}/>
                            <SearchHitContainer query={this.props.match.params.index}/>
                        </div>
                    </div>
                </div>
            </SearchkitProvider>
        );
    }

}

export default SearchPage;