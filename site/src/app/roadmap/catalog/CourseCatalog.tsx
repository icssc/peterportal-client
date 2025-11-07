// The "Catalog Tab" of the 3 tabs
// ...

import './CourseCatalog.scss';
import { useIsMobile } from '../../../helpers/util';
import { useNamedAcademicTerm } from '../../../hooks/namedAcademicTerm';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { setShowSearch } from '../../../store/slices/roadmapSlice';
import SavedAndSearch from '../search/SavedAndSearch';
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

export const CourseCatalogContent = () => {
  const selectedCourseList = useAppSelector((state) => state.courseRequirements.selectedTab);

  return (
    <div className="course-catalog">
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
