import './SearchSidebar.scss';

import CloseButton from 'react-bootstrap/CloseButton';
import SearchModule from '../../component/SearchModule/SearchModule';
import SearchHitContainer from '../../component/SearchHitContainer/SearchHitContainer';
import CourseHitItem from './CourseHitItem';

import { useAppDispatch } from '../../store/hooks';
import { setShowSearch } from '../../store/slices/roadmapSlice';
import { StrictModeDroppable } from './StrictModeDroppable';
import { useIsMobile } from '../../helpers/util';

const SearchSidebar = () => {
  const dispatch = useAppDispatch();
  const isMobile = useIsMobile();

  return (
    <div className="search-sidebar">
      {isMobile && (
        <div>
          <CloseButton
            className="close-icon"
            onClick={() => {
              dispatch(setShowSearch(false));
            }}
          />
        </div>
      )}
      <div className="search-body">
        <StrictModeDroppable droppableId="search" type="COURSE">
          {(provided) => {
            return (
              <div ref={provided.innerRef} style={{ height: '100%' }} {...provided.droppableProps}>
                <div className="search-sidebar-content">
                  <div className="search-sidebar-search-module">
                    <SearchModule index="courses" />
                  </div>
                  <SearchHitContainer index="courses" CourseHitItem={CourseHitItem} />
                </div>
                {provided.placeholder}
              </div>
            );
          }}
        </StrictModeDroppable>
      </div>
    </div>
  );
};

export default SearchSidebar;
