'use client';
import './SearchSidebar.scss';
import { useIsMobile } from '../../../helpers/util';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { setShowSearch } from '../../../store/slices/roadmapSlice';
import { Ref, useEffect, useRef } from 'react';
import UIOverlay from '../../../component/UIOverlay/UIOverlay';

import TransferCreditsMenu from '../transfers/TransferCreditsMenu';
import { loadMarkerCompletion } from '../../../helpers/courseRequirements';
import { useIsLoggedIn } from '../../../hooks/isLoggedIn';
import { initializeCompletedMarkers } from '../../../store/slices/courseRequirementsSlice';
import CourseCatalog from './CourseCatalog';

const SearchSidebar = ({ sidebarRef }: { sidebarRef: Ref<HTMLDivElement> }) => {
  const isMobile = useIsMobile();
  const isLoggedIn = useIsLoggedIn();
  const showSearch = useAppSelector((state) => state.roadmap.showSearch);
  const overlayRef = useRef<HTMLDivElement>(null);
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
    overlayRef.current?.classList.toggle('enter-done', showSearch);
  }, [isMobile, showSearch]);

  return (
    <>
      {isMobile && showSearch && <UIOverlay onClick={closeSearch} zIndex={449} ref={overlayRef} />}
      <CourseCatalog isMobile={isMobile} sidebarRef={sidebarRef} />
      {!isMobile && <TransferCreditsMenu />}
    </>
  );
};

export default SearchSidebar;
