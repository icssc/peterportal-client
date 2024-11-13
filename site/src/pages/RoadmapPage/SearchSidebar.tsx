import './SearchSidebar.scss';

import SearchHitContainer from '../../component/SearchHitContainer/SearchHitContainer';
import SearchModule from '../../component/SearchModule/SearchModule';
import CourseHitItem from './CourseHitItem';

import { useIsMobile } from '../../helpers/util';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setShowSearch } from '../../store/slices/roadmapSlice';
import { StrictModeDroppable } from './StrictModeDroppable';
import CourseBag from './CourseBag';
import { quarterDisplayNames } from '../../helpers/planner';
import { useEffect, useRef } from 'react';
import UIOverlay from '../../component/UIOverlay/UIOverlay';

const CloseRoadmapSearchButton = () => {
  const isMobile = useIsMobile();
  const planner = useAppSelector((state) => state.roadmap.plans[state.roadmap.currentPlanIndex].content.yearPlans);
  const { year, quarter } = useAppSelector((state) => state.roadmap.currentYearAndQuarter) || {};
  const dispatch = useAppDispatch();

  if (year == null || quarter == null) return <></>;

  const quarterName = quarterDisplayNames[planner[year].quarters[quarter].name];
  const yearName = planner[year].startYear + Number(quarterName === quarterDisplayNames.Fall);

  if (!isMobile) return <></>;

  const closeSearch = () => dispatch(setShowSearch({ show: false }));

  return (
    <button className="fixed" onClick={closeSearch}>
      Cancel Selecting for {quarterName} {yearName}
    </button>
  );
};

const SearchSidebar = () => {
  const isMobile = useIsMobile();
  const showSearch = useAppSelector((state) => state.roadmap.showSearch);
  const { showCourseBag } = useAppSelector((state) => state.roadmap);
  const overlayRef = useRef<HTMLDivElement>(null);
  const sidebarRef = useRef<HTMLDivElement>(null);
  const dispatch = useAppDispatch();

  const closeSearch = () => dispatch(setShowSearch({ show: false }));

  useEffect(() => {
    if (!isMobile) return;
    sidebarRef.current?.classList.toggle('enter-done', showSearch);
    overlayRef.current?.classList.toggle('enter-done', showSearch);
  }, [isMobile, showSearch]);

  return (
    <>
      {isMobile && showSearch && <UIOverlay onClick={closeSearch} zIndex={449} ref={overlayRef} />}
      <div className={`search-sidebar ${isMobile ? 'mobile' : ''}`} ref={sidebarRef}>
        <div className="search-sidebar-search-module">
          <SearchModule index="courses" />
        </div>
        <div className="search-body">
          {!showCourseBag ? (
            <StrictModeDroppable droppableId="search" type="COURSE">
              {(provided) => {
                return (
                  <div ref={provided.innerRef} style={{ height: '100%' }} {...provided.droppableProps}>
                    <div className="search-sidebar-content">
                      <SearchHitContainer index="courses" CourseHitItem={CourseHitItem} />
                    </div>
                    {provided.placeholder}
                  </div>
                );
              }}
            </StrictModeDroppable>
          ) : (
            <StrictModeDroppable droppableId="coursebag" type="COURSE">
              {(provided) => {
                return (
                  <div ref={provided.innerRef} style={{ height: '100%' }} {...provided.droppableProps}>
                    <div className="search-sidebar-content">
                      <CourseBag />
                    </div>
                    {provided.placeholder}
                  </div>
                );
              }}
            </StrictModeDroppable>
          )}
        </div>
        <CloseRoadmapSearchButton />
      </div>
    </>
  );
};

export default SearchSidebar;
