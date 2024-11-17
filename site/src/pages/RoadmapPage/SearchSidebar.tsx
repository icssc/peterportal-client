import './SearchSidebar.scss';

import SearchModule from '../../component/SearchModule/SearchModule';

import { useIsMobile } from '../../helpers/util';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setActiveCourse, setShowSearch } from '../../store/slices/roadmapSlice';
import { quarterDisplayNames } from '../../helpers/planner';
import { useEffect, useRef } from 'react';
import UIOverlay from '../../component/UIOverlay/UIOverlay';

import { ReactSortable, SortableEvent } from 'react-sortablejs';
import { useCoursebag } from '../../hooks/coursebag';
import { CourseGQLData } from '../../types/types';
import Course from './Course';
import { courseSearchSortable } from '../../helpers/sortable';

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

  const { coursebag } = useCoursebag();
  const results = useAppSelector((state) => state.search.courses.results);
  const shownCourses = JSON.parse(JSON.stringify(showCourseBag ? coursebag : results)) as CourseGQLData[];

  const closeSearch = () => dispatch(setShowSearch({ show: false }));

  useEffect(() => {
    if (!isMobile) return;
    sidebarRef.current?.classList.toggle('enter-done', showSearch);
    overlayRef.current?.classList.toggle('enter-done', showSearch);
  }, [isMobile, showSearch]);

  const setDraggedItem = (event: SortableEvent) => {
    const course = shownCourses[event.oldIndex!];
    dispatch(setActiveCourse(course));
  };

  return (
    <>
      {isMobile && showSearch && <UIOverlay onClick={closeSearch} zIndex={449} passedRef={overlayRef} />}
      <div className={`search-sidebar ${isMobile ? 'mobile' : ''}`} ref={sidebarRef}>
        <div className="search-sidebar-search-module">
          <SearchModule index="courses" />
        </div>
        <div className="search-body">
          <ReactSortable {...courseSearchSortable} list={shownCourses} onStart={setDraggedItem}>
            {shownCourses.map((course, i) => (
              <Course {...course} key={i} />
            ))}
          </ReactSortable>
        </div>
        <CloseRoadmapSearchButton />
      </div>
    </>
  );
};

export default SearchSidebar;
