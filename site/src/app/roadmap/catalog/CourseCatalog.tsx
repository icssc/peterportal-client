import './CourseCatalog.scss';
import { useIsMobile } from '../../../helpers/util';
import { useNamedAcademicTerm } from '../../../hooks/namedAcademicTerm';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { hideMobileCatalog } from '../../../store/slices/roadmapSlice';
import SavedAndSearch from '../search/SavedAndSearch';
import GERequiredCourseList from './GERequiredCourseList';
import MajorSelector from './MajorSelector';
import MinorSelector from './MinorSelector';
import RequirementsListSelector from './RequirementsListSelector';
import Library from './Library';

const CloseRoadmapSearchButton = () => {
  const isMobile = useIsMobile();
  const dispatch = useAppDispatch();
  const { year, quarter } = useNamedAcademicTerm();

  if (!isMobile) return <></>;

  const closeSearch = () => dispatch(hideMobileCatalog());

  return (
    <button className="fixed" onClick={closeSearch}>
      Cancel Selecting for {quarter} {year}
    </button>
  );
};

export const CourseCatalog = () => {
  const selectedCourseList = useAppSelector((state) => state.courseRequirements.selectedTab);

  return (
    <div className="course-catalog">
      <RequirementsListSelector />

      <div style={{ display: selectedCourseList === 'Major' ? 'block' : 'none' }}>
        <MajorSelector />
      </div>
      <div style={{ display: selectedCourseList === 'Minor' ? 'block' : 'none' }}>
        <MinorSelector />
      </div>
      <div style={{ display: selectedCourseList === 'GE' ? 'block' : 'none' }}>
        <GERequiredCourseList />
      </div>
      <div style={{ display: selectedCourseList === 'Library' ? 'block' : 'none' }}>
        <Library />
      </div>
      <div style={{ display: selectedCourseList === 'Search' ? 'block' : 'none' }}>
        <SavedAndSearch showSavedCoursesOnEmpty />
      </div>

      <CloseRoadmapSearchButton />
    </div>
  );
};
