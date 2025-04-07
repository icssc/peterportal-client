import './SearchSidebar.scss';

import { useIsMobile } from '../../helpers/util';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setShowSearch } from '../../store/slices/roadmapSlice';
import { useEffect, useRef } from 'react';
import UIOverlay from '../../component/UIOverlay/UIOverlay';

import { useNamedAcademicTerm } from '../../hooks/namedAcademicTerm';

import RequirementsListSelector from './sidebar/RequirementsListSelector';
import AllCourseSearch from './sidebar/AllCourseSearch';
import MajorRequiredCourseList from './sidebar/MajorRequiredCourseList';
import MinorRequiredCourseList from './sidebar/MinorRequiredCourseList';
import GERequiredCourseList from './sidebar/GERequiredCourseList';

const CloseRoadmapSearchButton = () => {
  const isMobile = useIsMobile();
  const dispatch = useAppDispatch();
  const { year, quarter } = useNamedAcademicTerm();

  if (!isMobile) return <></>;

  const closeSearch = () => dispatch(setShowSearch({ show: false }));

  return (
    <button className="fixed" onClick={closeSearch}>
      Cancel Selecting for {quarter} {year}
    </button>
  );
};

const SearchSidebar = () => {
  const isMobile = useIsMobile();
  const showSearch = useAppSelector((state) => state.roadmap.showSearch);
  const selectedCourseList = useAppSelector((state) => state.courseRequirements.selectedTab);
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
      {isMobile && showSearch && <UIOverlay onClick={closeSearch} zIndex={449} passedRef={overlayRef} />}
      <div className={`side-panel search-sidebar ${isMobile ? 'mobile' : ''}`} ref={sidebarRef}>
        <RequirementsListSelector />

        {selectedCourseList === 'Major' && <MajorRequiredCourseList />}
        {selectedCourseList === 'Minor' && <MinorRequiredCourseList />}
        {selectedCourseList === 'GE' && <GERequiredCourseList />}
        {selectedCourseList === 'Search' && <AllCourseSearch />}

        <CloseRoadmapSearchButton />
      </div>
    </>
  );
};

export default SearchSidebar;
