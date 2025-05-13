import './SearchSidebar.scss';

import { useIsMobile } from '../../helpers/util';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setShowSearch } from '../../store/slices/roadmapSlice';
import { useEffect, useRef } from 'react';
import UIOverlay from '../../component/UIOverlay/UIOverlay';

import { useNamedAcademicTerm } from '../../hooks/namedAcademicTerm';

import RequirementsListSelector from './sidebar/RequirementsListSelector';
import AllCourseSearch from './sidebar/AllCourseSearch';
import MajorSelector from './sidebar/MajorSelector';
import MinorSelector from './sidebar/MinorSelector';
import GERequiredCourseList from './sidebar/GERequiredCourseList';
import TransferCreditsMenu from './transfers/TransferCreditsMenu';
import { loadMarkerCompletion } from '../../helpers/courseRequirements';
import { useIsLoggedIn } from '../../hooks/isLoggedIn';
import { initializeCompletedMarkers } from '../../store/slices/courseRequirementsSlice';

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
  const isLoggedIn = useIsLoggedIn();
  const showSearch = useAppSelector((state) => state.roadmap.showSearch);
  const selectedCourseList = useAppSelector((state) => state.courseRequirements.selectedTab);
  const overlayRef = useRef<HTMLDivElement>(null);
  const sidebarRef = useRef<HTMLDivElement>(null);
  const dispatch = useAppDispatch();

  const closeSearch = () => dispatch(setShowSearch({ show: false }));

  // Load user-related Degree Requirements data (as opposed to AAPI-provided data)
  useEffect(() => {
    loadMarkerCompletion(isLoggedIn).then((completedMarkers) => {
      dispatch(initializeCompletedMarkers(completedMarkers));
    });
  }, [dispatch, isLoggedIn]);

  // Patch applying class names when a transition is triggered
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

        {selectedCourseList === 'Major' && <MajorSelector />}
        {selectedCourseList === 'Minor' && <MinorSelector />}
        {selectedCourseList === 'GE' && <GERequiredCourseList />}
        {selectedCourseList === 'Search' && <AllCourseSearch />}

        <CloseRoadmapSearchButton />
      </div>
      {!isMobile && <TransferCreditsMenu />}
    </>
  );
};

export default SearchSidebar;
