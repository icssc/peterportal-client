'use client';

import { useAppDispatch, useAppSelector } from '../../../../store/hooks';
import { setShowSearch } from '../../../../store/slices/roadmapSlice';
import { CourseCatalogContent } from './CourseCatalog';
import MobilePopup from '../../MobilePopup';

const MobileCourseCatalog = () => {
  const showSearch = useAppSelector((state) => state.roadmap.showSearch);
  const dispatch = useAppDispatch();

  /** @todo move out of global state since this will no longer be conditionally rendered */
  const closeSearch = () => dispatch(setShowSearch({ show: false }));

  return (
    <MobilePopup show={showSearch} onClose={closeSearch}>
      <CourseCatalogContent />
    </MobilePopup>
  );
};

export default MobileCourseCatalog;
