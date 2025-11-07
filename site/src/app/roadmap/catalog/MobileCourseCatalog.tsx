'use client';

import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { setShowSearch } from '../../../store/slices/roadmapSlice';
import { CourseCatalog } from './CourseCatalog';
import MobilePopup from '../MobilePopup';
import { useEffect } from 'react';
import { setSelectedTab } from '../../../store/slices/courseRequirementsSlice';
import { useIsMobile } from '../../../helpers/util';

const MobileCourseCatalog = () => {
  const showSearch = useAppSelector((state) => state.roadmap.showSearch);
  const selectedCourseList = useAppSelector((state) => state.courseRequirements.selectedTab);
  const isMobile = useIsMobile();
  const dispatch = useAppDispatch();

  /** @todo move out of global state since this will no longer be conditionally rendered */
  const closeSearch = () => dispatch(setShowSearch({ show: false }));

  useEffect(() => {
    if (isMobile && selectedCourseList === 'Saved') {
      dispatch(setSelectedTab('Search'));
    } else if (!isMobile && selectedCourseList === 'Search') {
      dispatch(setSelectedTab('Saved'));
    }
  }, [isMobile, selectedCourseList, dispatch]);

  return (
    <MobilePopup show={showSearch} onClose={closeSearch}>
      <CourseCatalog />
    </MobilePopup>
  );
};

export default MobileCourseCatalog;
