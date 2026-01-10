'use client';

import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { hideMobileCatalog } from '../../../store/slices/roadmapSlice';
import { CourseCatalog } from './CourseCatalog';
import { useCallback, useEffect, useRef } from 'react';
import MobilePopup from '../MobilePopup';
import { setSelectedTab } from '../../../store/slices/courseRequirementsSlice';
import { useIsMobile } from '../../../helpers/util';

const MobileCourseCatalog = () => {
  const showSearch = useAppSelector((state) => state.roadmap.showMobileCatalog);
  const selectedCourseList = useAppSelector((state) => state.courseRequirements.selectedTab);
  const isMobile = useIsMobile();
  const dispatch = useAppDispatch();
  const sidebarScrollContainerRef = useRef<HTMLDivElement>(null);

  const closeSearch = useCallback(() => dispatch(hideMobileCatalog()), [dispatch]);

  useEffect(() => {
    if (isMobile && selectedCourseList === 'Saved') {
      dispatch(setSelectedTab('Search'));
    } else if (!isMobile && selectedCourseList === 'Search') {
      dispatch(setSelectedTab('Saved'));
    }
  }, [isMobile, selectedCourseList, dispatch]);

  useEffect(() => {
    if (!isMobile) closeSearch();
  }, [isMobile, closeSearch]);

  return (
    // <MobilePopup show={showSearch} onClose={closeSearch} ref={sidebarScrollContainerRef}>
    <MobilePopup show={showSearch} onClose={closeSearch}>
      <CourseCatalog sidebarScrollContainerRef={sidebarScrollContainerRef} />
    </MobilePopup>
  );
};

export default MobileCourseCatalog;
