import { FC } from 'react';
import SearchModule from '../../../component/SearchModule/SearchModule';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { useCoursebag } from '../../../hooks/coursebag';
import { CourseGQLData } from '../../../types/types';
import { deepCopy, useIsMobile } from '../../../helpers/util';
import { ReactSortable, SortableEvent } from 'react-sortablejs';
import { setActiveCourse } from '../../../store/slices/roadmapSlice';
import { getAllCoursesFromPlan, validateCourse } from '../../../helpers/planner';
import { courseSearchSortable } from '../../../helpers/sortable';
import Course from '../Course';
import { Spinner } from 'react-bootstrap';
import noResultsImg from '../../../asset/no-results-crop.webp';

interface SearchPlaceholderProps {
  searchInProgress: boolean;
  showCourseBag: boolean;
}

const SearchPlaceholder = ({ searchInProgress, showCourseBag }: SearchPlaceholderProps) => {
  if (searchInProgress) return <Spinner animation="border" role="status" />;

  const placeholderText = showCourseBag
    ? 'No courses saved. Try searching for something!'
    : "Sorry, we couldn't find any results for that search!";

  return (
    <>
      <img src={noResultsImg} alt="No results found" />
      {placeholderText}
    </>
  );
};

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

  const roadmap = useAppSelector((state) => state.roadmap);
  const allExistingCourses = getAllCoursesFromPlan(roadmap?.plans[roadmap.currentPlanIndex].content);
  const transfers = roadmap?.transfers.map((transfer) => transfer.name);
  const clearedCourses = new Set([...allExistingCourses, ...transfers]);

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
            const missingPrerequisites = Array.from(
              validateCourse(clearedCourses, course.prerequisiteTree, new Set(), course.corequisites),
            );
            const requiredCourses = missingPrerequisites.length ? missingPrerequisites : undefined;
            return (
              <Course
                data={course}
                key={i}
                addMode={isMobile ? 'tap' : 'drag'}
                openPopoverLeft={true}
                requiredCourses={requiredCourses}
              />
            );
          })}
        </ReactSortable>
      ) : (
        <div className="no-results">
          <SearchPlaceholder searchInProgress={searchInProgress} showCourseBag={showCourseBag} />
        </div>
      )}
    </>
  );
};

export default AllCourseSearch;
