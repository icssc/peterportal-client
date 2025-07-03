import { FC } from 'react';
import SearchModule from '../../../component/SearchModule/SearchModule';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { useSavedCourses } from '../../../hooks/savedCourses';
import { CourseGQLData } from '../../../types/types';
import { deepCopy, useIsMobile } from '../../../helpers/util';
import { ReactSortable, SortableEvent } from 'react-sortablejs';
import { setActiveCourse } from '../../../store/slices/roadmapSlice';
import { getMissingPrerequisites } from '../../../helpers/planner';
import { courseSearchSortable } from '../../../helpers/sortable';
import Course from '../Course';
import LoadingSpinner from '../../../component/LoadingSpinner/LoadingSpinner';
import noResultsImg from '../../../asset/no-results-crop.webp';
import { useClearedCourses } from '../../../hooks/planner';

interface SearchPlaceholderProps {
  searchInProgress: boolean;
  showSavedCourses: boolean;
}

const SearchPlaceholder = ({ searchInProgress, showSavedCourses }: SearchPlaceholderProps) => {
  if (searchInProgress) return <LoadingSpinner />;

  const placeholderText = showSavedCourses
    ? 'No courses saved. Try searching for something!'
    : "Sorry, we couldn't find any results for that search!";

  return (
    <div className="no-results">
      <img src={noResultsImg} alt="No results found" />
      {placeholderText}
    </div>
  );
};

const AllCourseSearch: FC = () => {
  const { showSavedCourses } = useAppSelector((state) => state.roadmap);
  const { results, searchInProgress } = useAppSelector((state) => state.search.courses);
  const { savedCourses } = useSavedCourses();
  const isMobile = useIsMobile();
  const dispatch = useAppDispatch();

  // Deep copy because Sortable requires data to be extensible (non read-only)
  const shownCourses = deepCopy(showSavedCourses ? savedCourses : results) as CourseGQLData[];
  const setDraggedItem = (event: SortableEvent) => {
    const course = shownCourses[event.oldIndex!];
    dispatch(setActiveCourse(course));
  };

  const clearedCourses = useClearedCourses();

  return (
    <>
      <div className="search-sidebar-search-module">
        <SearchModule index="courses" />
      </div>
      <h3 className="saved-courses-title">{showSavedCourses ? 'Saved Courses' : 'Search Results'}</h3>

      {!searchInProgress && shownCourses.length ? (
        <ReactSortable
          {...courseSearchSortable}
          list={shownCourses}
          onStart={setDraggedItem}
          disabled={isMobile}
          className={'search-body' + (isMobile ? ' disabled' : '')}
        >
          {shownCourses.map((course, i) => {
            const missingPrerequisites = getMissingPrerequisites(clearedCourses, course);
            return (
              <Course
                data={course}
                key={i}
                addMode={isMobile ? 'tap' : 'drag'}
                openPopoverLeft={true}
                requiredCourses={missingPrerequisites}
              />
            );
          })}
        </ReactSortable>
      ) : (
        <SearchPlaceholder searchInProgress={searchInProgress} showSavedCourses={showSavedCourses} />
      )}
    </>
  );
};

export default AllCourseSearch;
