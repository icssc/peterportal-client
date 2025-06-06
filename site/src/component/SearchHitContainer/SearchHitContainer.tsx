import { useEffect, FC, useRef } from 'react';
import './SearchHitContainer.scss';

import { SearchIndex, CourseGQLData, ProfessorGQLData, SearchResultData } from '../../types/types';
import SearchPagination from '../SearchPagination/SearchPagination';
import NoResults from '../NoResults/NoResults';
import LoadingSpinner from '../LoadingSpinner/LoadingSpinner';
import CourseHitItem from '../../pages/SearchPage/CourseHitItem';
import ProfessorHitItem from '../../pages/SearchPage/ProfessorHitItem';
import { getMissingPrerequisites } from '../../helpers/planner';
import { useAppSelector } from '../../store/hooks';
import { useClearedCourses } from '../../hooks/planner';

interface SearchResultsProps {
  index: SearchIndex;
  results: SearchResultData;
}

const SearchResults: FC<SearchResultsProps> = ({ index, results }) => {
  const clearedCourses = useClearedCourses();

  if (index === 'courses') {
    return (results as CourseGQLData[]).map((course) => {
      const requiredCourses = getMissingPrerequisites(clearedCourses, course);
      return <CourseHitItem key={course.id} course={course} requiredCourses={requiredCourses} />;
    });
  }

  return (results as ProfessorGQLData[]).map((professor) => (
    <ProfessorHitItem key={professor.ucinetid} professor={professor} />
  ));
};

interface SearchHitContainerProps {
  index: SearchIndex;
}

const SearchHitContainer: FC<SearchHitContainerProps> = ({ index }) => {
  const { query, results, searchInProgress } = useAppSelector((state) => state.search[index]);
  const containerDivRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    containerDivRef.current!.scrollTop = 0;
  }, [results]);

  return (
    <div ref={containerDivRef} className="search-hit-container">
      {searchInProgress ? (
        <LoadingSpinner />
      ) : results.length === 0 ? (
        <NoResults
          notSearching={query === ''}
          placeholderText={`Start typing in the search bar to search for ${
            index === 'courses' ? 'courses' : 'professors'
          }...`}
        />
      ) : (
        <>
          <SearchResults index={index} results={results} />
          <SearchPagination index={index} />
        </>
      )}
    </div>
  );
};

export default SearchHitContainer;
