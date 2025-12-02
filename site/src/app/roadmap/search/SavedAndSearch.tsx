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

const ResultsHeader: FC = () => {
  const showSavedCourses = useAppSelector((state) => state.roadmap.showSavedCourses);
  const viewIndex = useAppSelector((state) => state.search.viewIndex);
  const isMobile = useIsMobile();
  const dispatch = useAppDispatch();

  const singularIndexType = viewIndex.replace(/s$/, '');
  const otherIndexType: SearchIndex = viewIndex === 'courses' ? 'professors' : 'courses';

  const resultsOther = useAppSelector((state) => state.search[otherIndexType].results);

  if (showSavedCourses) {
    return <h3 className="results-list-title">Saved Courses</h3>;
  }

  if (isMobile) {
    return <h3 className="results-list-title">Search Results</h3>;
  }

  const switchViewIndex = (event: React.MouseEvent) => {
    event.preventDefault();
    dispatch(setSearchViewIndex(otherIndexType));
  };

  return (
    <p className="result-type-header">
      Showing {singularIndexType} results.
      {resultsOther.length > 0 && (
        <a className="results-switcher" href={`#${otherIndexType}`} onClick={switchViewIndex}>
          {' '}
          Show {otherIndexType}
        </a>
      )}
    </p>
  );
};

const SavedAndSearch: FC = () => {
  const { showSavedCourses } = useAppSelector((state) => state.roadmap);
  const viewIndex = useAppSelector((state) => state.search.viewIndex);
  const results = useAppSelector((state) => state.search[viewIndex].results);
  const searchInProgress = useAppSelector((state) => state.search.inProgressSearchOperation !== 'none');
  const { savedCourses } = useSavedCourses();
  const showFullscreenSearch = useAppSelector((state) => state.roadmap.showMobileFullscreenSearch);

  // Deep copy because Sortable requires data to be extensible (non read-only)
  const searchResults = deepCopy(showSavedCourses ? savedCourses : results); // as CourseGQLData[];

  return (
    <>
      {!showFullscreenSearch && (
        <>
          <SearchModule />
          <ResultsHeader />
        </>
      )}

      {searchInProgress ? (
        <LoadingSpinner />
      ) : searchResults.length === 0 ? (
        <NoResults showPrompt={showSavedCourses} prompt="No courses saved. Try searching for something!" />
      ) : !showSavedCourses && viewIndex === 'professors' ? (
        <ProfessorResultsContainer searchResults={searchResults as ProfessorGQLData[]} />
      ) : (
        <CourseResultsContainer searchResults={searchResults as CourseGQLData[]} />
      )}
    </>
  );
};

export default SavedAndSearch;
