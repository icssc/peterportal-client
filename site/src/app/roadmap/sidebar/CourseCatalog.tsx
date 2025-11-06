// The "Catalog Tab" of the 3 tabs
// ...

import { FC, Ref } from 'react';
import { useIsMobile } from '../../../helpers/util';
import { useNamedAcademicTerm } from '../../../hooks/namedAcademicTerm';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { setShowSearch } from '../../../store/slices/roadmapSlice';
import SavedAndSearch from './catalog/SavedAndSearch';
import GERequiredCourseList from './GERequiredCourseList';
import MajorSelector from './MajorSelector';
import MinorSelector from './MinorSelector';
import RequirementsListSelector from './RequirementsListSelector';

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

interface CourseCatalogProps {
  isMobile: boolean;
  sidebarRef: Ref<HTMLDivElement>;
}

const CourseCatalog: FC<CourseCatalogProps> = ({ isMobile, sidebarRef }) => {
  const selectedCourseList = useAppSelector((state) => state.courseRequirements.selectedTab);

  return (
    <div className={`side-panel search-sidebar ${isMobile ? 'mobile' : ''}`} ref={sidebarRef}>
      <RequirementsListSelector />

      {selectedCourseList === 'Major' && <MajorSelector />}
      {selectedCourseList === 'Minor' && <MinorSelector />}
      {selectedCourseList === 'GE' && <GERequiredCourseList />}
      {selectedCourseList === 'Saved' && <SavedAndSearch /> /** @todo v2 saved tab */}
      {selectedCourseList === 'Search' && <SavedAndSearch />}

      <CloseRoadmapSearchButton />
    </div>
  );
};

export default CourseCatalog;
