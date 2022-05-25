import React, { FC } from "react";
import "./SearchSidebar.scss";
import { Droppable } from "react-beautiful-dnd";
import { isMobile, isBrowser } from 'react-device-detect';

import CloseButton from 'react-bootstrap/CloseButton';
import SearchModule from '../../component/SearchModule/SearchModule';
import SearchHitContainer from '../../component/SearchHitContainer/SearchHitContainer';
import CourseHitItem from "./CourseHitItem";

import { useAppDispatch } from '../../store/hooks';
import { setShowSearch } from '../../store/slices/roadmapSlice';

const SearchSidebar = () => {
  const dispatch = useAppDispatch();
  return (
    <div className="search-sidebar" >
      {isMobile && <div><CloseButton className='close-icon' onClick={() => { dispatch(setShowSearch(false)) }} /></div>}
      <div className='search-body'>
        <Droppable droppableId="search" type="COURSE">
          {(provided) => {
            return (
              <div
                ref={provided.innerRef}
                style={{ height: "100%" }}
                {...provided.droppableProps}
              >
                <div className='search-sidebar-content'>
                  <div className='search-sidebar-search-module'>
                    <SearchModule index='courses' />
                  </div>
                  <SearchHitContainer index='courses' CourseHitItem={CourseHitItem} />
                </div>
                {provided.placeholder}
              </div>
            );
          }}
        </Droppable>
      </div>
    </div>
  );
};


export default SearchSidebar;