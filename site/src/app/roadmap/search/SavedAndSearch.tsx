import './SavedAndSearch.scss';
import React, { FC } from 'react';
import SearchModule from '../../../component/SearchModule/SearchModule';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { useSavedCourses } from '../../../hooks/savedCourses';
import { CourseGQLData, ProfessorGQLData, SearchIndex } from '../../../types/types';
import { deepCopy, useIsMobile } from '../../../helpers/util';
import { ReactSortable, SortableEvent } from 'react-sortablejs';
import { setActiveCourse } from '../../../store/slices/roadmapSlice';
import { getMissingPrerequisites } from '../../../helpers/planner';
import { courseSearchSortable } from '../../../helpers/sortable';
import Course from '../planner/Course';
import LoadingSpinner from '../../../component/LoadingSpinner/LoadingSpinner';
import NoResults from '../../../component/NoResults/NoResults';
import { useClearedCourses } from '../../../hooks/planner';
import ProfessorResult from './ProfessorResult';
import { setSearchViewIndex } from '../../../store/slices/searchSlice';
import SearchFilters from '../../../component/SearchFilters/SearchFilters';

interface CourseResultsContainerProps {
  searchResults: CourseGQLData[];
}

const CourseResultsContainer: FC<CourseResultsContainerProps> = ({ searchResults }) => {
  const isMobile = useIsMobile();
  const clearedCourses = useClearedCourses();
  const dispatch = useAppDispatch();

  const setDraggedItem = (event: SortableEvent) => {
    const course = searchResults[event.oldIndex!];
    dispatch(setActiveCourse({ course }));
  };

  // Deep copy because Sortable requires data to be extensible (non read-only). Must be done within component
  searchResults = deepCopy(searchResults);

  return (
    <ReactSortable
      {...courseSearchSortable}
      list={searchResults}
      onStart={setDraggedItem}
      disabled={isMobile}
      /**
       * @todo merge classNames for `roadmap-search-results` for courses + profs after getting
       * rid of independent search pages
       */
      className={'roadmap-search-results' + (isMobile ? ' disabled' : '')}
    >
      {searchResults.map((course, i) => {
        const missingPrerequisites = getMissingPrerequisites(clearedCourses, course.prerequisiteTree);
        return (
          <Course data={course} key={i} addMode={isMobile ? 'tap' : 'drag'} requiredCourses={missingPrerequisites} />
        );
      })}
    </ReactSortable>
  );
};

interface ProfessorResultsContainerProps {
  searchResults: ProfessorGQLData[];
}

const ProfessorResultsContainer: FC<ProfessorResultsContainerProps> = ({ searchResults }) => {
  return (
    <div className="professor-results">
      {searchResults.map((prof) => {
        return <ProfessorResult key={prof.ucinetid} data={prof} />;
      })}
    </div>
  );
};

interface ShowSavedProps {
  showSavedCoursesOnEmpty?: boolean;
}

export const ResultsHeader: FC<ShowSavedProps> = ({ showSavedCoursesOnEmpty }) => {
  const inProgressSearch = useAppSelector((state) => state.search.inProgressSearchOperation);
  const showSavedCourses = useAppSelector((state) => !!showSavedCoursesOnEmpty && state.roadmap.showSavedCourses);
  const viewIndex = useAppSelector((state) => state.search.viewIndex);
  const showMobileCatalog = useAppSelector((state) => state.roadmap.showMobileCatalog);
  const dispatch = useAppDispatch();

  const singularIndexType = viewIndex.replace(/s$/, '');
  const otherIndexType: SearchIndex = viewIndex === 'courses' ? 'professors' : 'courses';

  const resultsOther = useAppSelector((state) => state.search[otherIndexType].results);

  if (inProgressSearch === 'newQuery') return null;

  if (showSavedCourses) {
    return <h3 className="results-list-title">Saved Courses</h3>;
  }

  if (showMobileCatalog) {
    return <h3 className="results-list-title">Search Results</h3>;
  }

  const switchViewIndex = (event: React.MouseEvent) => {
    event.preventDefault();
    dispatch(setSearchViewIndex(otherIndexType));
  };

  return (
    <p className="result-type-header">
      Showing {singularIndexType} results.{' '}
      {resultsOther.length > 0 && (
        <a className="results-switcher" href={`#${otherIndexType}`} onClick={switchViewIndex}>
          Show {otherIndexType}
        </a>
      )}
    </p>
  );
};

const SavedAndSearch: FC<ShowSavedProps> = ({ showSavedCoursesOnEmpty }) => {
  const showSavedCourses = useAppSelector((state) => !!showSavedCoursesOnEmpty && state.roadmap.showSavedCourses);
  const showMobileCatalog = useAppSelector((state) => state.roadmap.showMobileCatalog);
  const viewIndex = useAppSelector((state) => (showMobileCatalog ? 'courses' : state.search.viewIndex));
  const results = useAppSelector((state) => state.search[viewIndex].results);
  const hasQuery = useAppSelector((state) => !!state.search[viewIndex].query);
  const inProgressSearch = useAppSelector((state) => state.search.inProgressSearchOperation);
  const { savedCourses } = useSavedCourses();

  const searchResults = showSavedCourses ? savedCourses : results;

  const showCustomPrompt = showSavedCourses || !hasQuery;
  const customPrompt = showSavedCourses
    ? 'No courses saved. Try searching for something!'
    : 'Start typing in the search bar to search for courses or instructors...';

  const noResults = <NoResults showPrompt={showCustomPrompt} prompt={customPrompt} />;

  const showHeader = showSavedCoursesOnEmpty || hasQuery;
  const showCourseFilters = hasQuery && viewIndex === 'courses' && inProgressSearch !== 'newQuery';

  return (
    <>
      <SearchModule />
      {showHeader && <ResultsHeader showSavedCoursesOnEmpty />}
      {showCourseFilters && <SearchFilters />}

      {inProgressSearch !== 'none' ? (
        <LoadingSpinner />
      ) : !showSavedCourses && viewIndex === 'professors' ? (
        <>
          {searchResults.length === 0 && noResults}
          <ProfessorResultsContainer searchResults={searchResults as ProfessorGQLData[]} />
        </>
      ) : (
        <>
          {searchResults.length === 0 && noResults}
          <CourseResultsContainer searchResults={searchResults as CourseGQLData[]} />
        </>
      )}
    </>
  );
};

export default SavedAndSearch;
