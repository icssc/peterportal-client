import './SearchSidebar.scss';

import SearchModule from '../../component/SearchModule/SearchModule';

import { deepCopy, useIsMobile } from '../../helpers/util';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setActiveCourse, setShowSearch } from '../../store/slices/roadmapSlice';
import { useEffect, useRef } from 'react';
import UIOverlay from '../../component/UIOverlay/UIOverlay';

import { ReactSortable, SortableEvent } from 'react-sortablejs';
import { useCoursebag } from '../../hooks/coursebag';
import { CourseGQLData } from '../../types/types';
import Course from './Course';
import { courseSearchSortable } from '../../helpers/sortable';
import { Spinner } from 'react-bootstrap';
import { useNamedAcademicTerm } from '../../hooks/namedAcademicTerm';
import noResultsImg from '../../asset/no-results-crop.webp';

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

const SearchSidebar = () => {
  const isMobile = useIsMobile();
  const showSearch = useAppSelector((state) => state.roadmap.showSearch);
  const { showCourseBag } = useAppSelector((state) => state.roadmap);
  const overlayRef = useRef<HTMLDivElement>(null);
  const sidebarRef = useRef<HTMLDivElement>(null);
  const dispatch = useAppDispatch();

  const { coursebag } = useCoursebag();
  const { results, searchInProgress } = useAppSelector((state) => state.search.courses);

  // Deep copy because Sortable requires data to be extensible (non read-only)
  const shownCourses = deepCopy(showCourseBag ? coursebag : results) as CourseGQLData[];

  const closeSearch = () => dispatch(setShowSearch({ show: false }));

  useEffect(() => {
    if (!isMobile) return;
    sidebarRef.current?.classList.toggle('enter-done', showSearch);
    overlayRef.current?.classList.toggle('enter-done', showSearch);
  }, [isMobile, showSearch]);

  const setDraggedItem = (event: SortableEvent) => {
    const course = shownCourses[event.oldIndex!];
    dispatch(setActiveCourse(course));
  };

  return (
    <>
      {isMobile && showSearch && <UIOverlay onClick={closeSearch} zIndex={449} passedRef={overlayRef} />}
      <div className={`search-sidebar ${isMobile ? 'mobile' : ''}`} ref={sidebarRef}>
        <div className="search-sidebar-search-module">
          <SearchModule index="courses" />
        </div>
        {showCourseBag && <h3 className="coursebag-title">Saved Courses</h3>}

        {!searchInProgress && shownCourses.length ? (
          <ReactSortable
            {...courseSearchSortable}
            list={shownCourses}
            onStart={setDraggedItem}
            disabled={isMobile}
            className={'search-body' + (isMobile ? ' disabled' : '')}
          >
            {shownCourses.map((course, i) => (
              <Course {...course} key={i} addMode={isMobile ? 'tap' : 'drag'} openPopoverLeft={true} />
            ))}
          </ReactSortable>
        ) : (
          <div className="no-results">
            <SearchPlaceholder searchInProgress={searchInProgress} showCourseBag={showCourseBag} />
          </div>
        )}
        <CloseRoadmapSearchButton />
      </div>
    </>
  );
};

export default SearchSidebar;
