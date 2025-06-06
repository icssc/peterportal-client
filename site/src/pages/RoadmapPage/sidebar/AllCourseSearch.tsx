import { FC } from 'react';
import { ReactSortable, SortableEvent } from 'react-sortablejs';

import Course from '../Course';
import SearchModule from '../../../component/SearchModule/SearchModule';
import LoadingSpinner from '../../../component/LoadingSpinner/LoadingSpinner';
import NoResults from '../../../component/NoResults/NoResults';

import { CourseGQLData } from '../../../types/types';
import { setActiveCourse } from '../../../store/slices/roadmapSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { useCoursebag } from '../../../hooks/coursebag';
import { useClearedCourses } from '../../../hooks/planner';
import { getMissingPrerequisites } from '../../../helpers/planner';
import { courseSearchSortable } from '../../../helpers/sortable';
import { deepCopy, useIsMobile } from '../../../helpers/util';

const AllCourseSearch: FC = () => {
  const { showCourseBag } = useAppSelector((state) => state.roadmap);
  const { results, searchInProgress } = useAppSelector((state) => state.search.courses);
  const { coursebag } = useCoursebag();
  const clearedCourses = useClearedCourses();
  const isMobile = useIsMobile();
  const dispatch = useAppDispatch();

  // Deep copy because Sortable requires data to be extensible (non read-only)
  const shownCourses = deepCopy(showCourseBag ? coursebag : results) as CourseGQLData[];
  const setDraggedItem = (event: SortableEvent) => {
    const course = shownCourses[event.oldIndex!];
    dispatch(setActiveCourse(course));
  };

  return (
    <>
      <SearchModule index="courses" />
      <h3 className="coursebag-title">{showCourseBag ? 'Saved Courses' : 'Search Results'}</h3>
      {searchInProgress ? (
        <LoadingSpinner />
      ) : shownCourses.length === 0 ? (
        <NoResults notSearching={showCourseBag} placeholderText="No courses saved. Try searching for something!" />
      ) : (
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
      )}
    </>
  );
};

export default AllCourseSearch;
