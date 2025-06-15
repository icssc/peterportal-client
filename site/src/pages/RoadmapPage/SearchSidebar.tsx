import { useEffect } from 'react';
import './SearchSidebar.scss';

import UIOverlay from '../../component/UIOverlay/UIOverlay';
import RequirementsListSelector from './sidebar/RequirementsListSelector';
import AllCourseSearch from './sidebar/AllCourseSearch';
import MajorSelector from './sidebar/MajorSelector';
import MinorSelector from './sidebar/MinorSelector';
import GERequiredCourseList from './sidebar/GERequiredCourseList';
import TransferCreditsMenu from './transfers/TransferCreditsMenu';

import { setShowSearch } from '../../store/slices/roadmapSlice';
import { initializeCompletedMarkers } from '../../store/slices/courseRequirementsSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { useNamedAcademicTerm } from '../../hooks/namedAcademicTerm';
import { useIsLoggedIn } from '../../hooks/isLoggedIn';
import { useToggleRef } from '../../hooks/planner';
import { loadMarkerCompletion } from '../../helpers/courseRequirements';
import { useIsMobile } from '../../helpers/util';

const SearchSidebar = () => {
  const isMobile = useIsMobile();
  const isLoggedIn = useIsLoggedIn();
  const showSearch = useAppSelector((state) => state.roadmap.showSearch);
  const selectedCourseList = useAppSelector((state) => state.courseRequirements.selectedTab);
  const { overlayRef, sidebarRef } = useToggleRef(isMobile, showSearch);
  const { year, quarter } = useNamedAcademicTerm();
  const dispatch = useAppDispatch();

  const closeSearch = () => dispatch(setShowSearch({ show: false }));

  // Load user-related Degree Requirements data (as opposed to AAPI-provided data)
  useEffect(() => {
    loadMarkerCompletion(isLoggedIn).then((completedMarkers) => {
      dispatch(initializeCompletedMarkers(completedMarkers));
    });
  }, [dispatch, isLoggedIn]);

  const courseListComponentMap = {
    Major: <MajorSelector />,
    Minor: <MinorSelector />,
    GE: <GERequiredCourseList />,
    Search: <AllCourseSearch />,
  };

  const CloseRoadmapSearchButton = () => (
    <button className="fixed" onClick={closeSearch}>
      Cancel Selecting for {quarter} {year}
    </button>
  );

  return (
    <>
      {isMobile && showSearch && <UIOverlay onClick={closeSearch} zIndex={449} passedRef={overlayRef} />}
      <div className={`side-panel search-sidebar ${isMobile ? 'mobile' : ''}`} ref={sidebarRef}>
        <RequirementsListSelector />
        {courseListComponentMap[selectedCourseList]}
        {isMobile && <CloseRoadmapSearchButton />}
      </div>
      {!isMobile && <TransferCreditsMenu />}
    </>
  );
};

export default SearchSidebar;
