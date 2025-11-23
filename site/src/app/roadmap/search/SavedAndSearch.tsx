import './SavedAndSearch.scss';
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
import Course from '../planner/Course';
import LoadingSpinner from '../../../component/LoadingSpinner/LoadingSpinner';
import NoResults from '../../../component/NoResults/NoResults';
import { useClearedCourses } from '../../../hooks/planner';

const SavedAndSearch: FC = () => {
  const { showSavedCourses } = useAppSelector((state) => state.roadmap);
  const { results } = useAppSelector((state) => state.search.courses);
  const searchInProgress = useAppSelector((state) => state.search.searchInProgress);
  const { savedCourses } = useSavedCourses();
  const isMobile = useIsMobile();
  const dispatch = useAppDispatch();

  // Deep copy because Sortable requires data to be extensible (non read-only)
  const shownCourses = deepCopy(showSavedCourses ? savedCourses : results) as CourseGQLData[];
  const setDraggedItem = (event: SortableEvent) => {
    const course = shownCourses[event.oldIndex!];
    dispatch(setActiveCourse({ course }));
  };

  const clearedCourses = useClearedCourses();

  return (
    <>
      <SearchModule index="courses" />
      <h3 className="saved-courses-title">{showSavedCourses ? 'Saved Courses' : 'Search Results'}</h3>

      {searchInProgress ? (
        <LoadingSpinner />
      ) : shownCourses.length === 0 ? (
        <NoResults showPrompt={showSavedCourses} prompt="No courses saved. Try searching for something!" />
      ) : (
        <ReactSortable
          {...courseSearchSortable}
          list={shownCourses}
          onStart={setDraggedItem}
          disabled={isMobile}
          className={'roadmap-search-results' + (isMobile ? ' disabled' : '')}
        >
          {shownCourses.map((course, i) => {
            const missingPrerequisites = getMissingPrerequisites(clearedCourses, course.prerequisiteTree);
            return (
              <Course
                data={course}
                key={i}
                addMode={isMobile ? 'tap' : 'drag'}
                requiredCourses={missingPrerequisites}
              />
            );
          })}
        </ReactSortable>
      )}
    </>
  );
};

export default SavedAndSearch;
