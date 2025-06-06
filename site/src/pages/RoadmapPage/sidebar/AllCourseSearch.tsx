import { FC } from 'react';
import SearchModule from '../../../component/SearchModule/SearchModule';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { useCoursebag } from '../../../hooks/coursebag';
import { CourseGQLData } from '../../../types/types';
import { deepCopy, useIsMobile } from '../../../helpers/util';
import { ReactSortable, SortableEvent } from 'react-sortablejs';
import { setActiveCourse } from '../../../store/slices/roadmapSlice';
import { getMissingPrerequisites } from '../../../helpers/planner';
import { courseSearchSortable } from '../../../helpers/sortable';
import Course from '../Course';
import { Spinner } from 'react-bootstrap';
import NoResults from '../../../NoResults/NoResults';
import { useClearedCourses } from '../../../hooks/planner';

const AllCourseSearch: FC = () => {
  const { showCourseBag } = useAppSelector((state) => state.roadmap);
  const { results, searchInProgress } = useAppSelector((state) => state.search.courses);
  const { coursebag } = useCoursebag();
  const isMobile = useIsMobile();
  const dispatch = useAppDispatch();

  // Deep copy because Sortable requires data to be extensible (non read-only)
  const shownCourses = deepCopy(showCourseBag ? coursebag : results) as CourseGQLData[];
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
      <h3 className="coursebag-title">{showCourseBag ? 'Saved Courses' : 'Search Results'}</h3>

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
      ) : searchInProgress ? (
        <Spinner animation="border" role="status" />
      ) : (
        <NoResults notSearching={showCourseBag} placeholderText="No courses saved. Try searching for something!" />
      )}
    </>
  );
};

export default AllCourseSearch;
