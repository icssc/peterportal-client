import React from "react";
import "./SearchSidebar.scss";
import { PlusCircle } from "react-bootstrap-icons";
import { Droppable } from "react-beautiful-dnd";

import SearchModule from './SearchModule'
import SearchHitContainer from './SearchHitContainer'
import { SearchkitComponent, SearchkitManager, SearchkitProvider, SearchkitComponentProps } from 'searchkit';

const SearchSidebar = () => {
  return (
    <div className="search-sidebar">
      <div className='search-header'>
        <h1 className="search-title">Add a course</h1>
        <PlusCircle className="plus-circle" />
        <h1 className="custom-button">Custom</h1>
      </div>
      <div className='search-body'>
        <Droppable droppableId="search" type="COURSE">
          {(provided) => {
            return (
              <div
                ref={provided.innerRef}
                {...provided.droppableProps}
              >
                <Search />
                {provided.placeholder}
              </div>
            );
          }}
        </Droppable>
      </div>
    </div>
  );
};

class Search extends SearchkitComponent<SearchkitComponentProps, {}> {
  render() {
    // 'this.props.match.params.index' is used to determine which index to 
    // query via url location - i.e: (professor || courses)
    let searchkit = new SearchkitManager('/courses');

    return (
      <SearchkitProvider searchkit={searchkit}>
        <>
          <div style={{ display: 'flex', flexGrow: 1 }}>
            <div style={{ overflow: 'scroll', height: '100vh', width: '55vw', overflowX: 'hidden' }}>
              <SearchModule query={'courses'} />
              <SearchHitContainer query={'courses'} />
            </div>
          </div>
        </>
      </SearchkitProvider>
    );
  }
}


export default SearchSidebar;