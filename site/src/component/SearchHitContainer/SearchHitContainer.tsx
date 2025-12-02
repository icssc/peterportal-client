import { useEffect, FC, useRef } from 'react';
import './SearchHitContainer.scss';

import { useAppSelector } from '../../store/hooks';

import { SearchIndex, CourseGQLData, ProfessorGQLData, SearchResultData } from '../../types/types';
import SearchPagination from '../SearchPagination/SearchPagination';
import NoResults from '../NoResults/NoResults';
import LoadingSpinner from '../LoadingSpinner/LoadingSpinner';
import CourseHitItem from '../../app/search/CourseHitItem';
import ProfessorHitItem from '../../app/search/ProfessorHitItem';

interface SearchHitContainerProps {
  index: SearchIndex;
}

const SearchResults = ({ index, results }: Required<SearchHitContainerProps> & { results: SearchResultData }) => {
  if (index === 'courses') {
    return (results as CourseGQLData[]).map((course) => {
      return <CourseHitItem key={course.id} {...course} />;
    });
  } else {
    return (results as ProfessorGQLData[]).map((professor) => (
      <ProfessorHitItem key={professor.ucinetid} {...professor} />
    ));
  }
};

const SearchHitContainer: FC<SearchHitContainerProps> = ({ index }) => {
  const { query, results } = useAppSelector((state) => state.search[index]);
  const searchInProgress = useAppSelector((state) => state.search.inProgressSearchOperation !== 'none');
  const containerDivRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    containerDivRef.current!.scrollTop = 0;
  }, [results]);

  return (
    <div ref={containerDivRef} className="search-hit-container">
      {searchInProgress && <LoadingSpinner />}
      {!searchInProgress && (!query || results.length === 0) && (
        <NoResults showPrompt={query === ''} prompt={`Start typing in the search bar to search for ${index}...`} />
      )}
      {!searchInProgress && query && results.length > 0 && (
        <>
          <SearchResults index={index} results={results} />
          <div className="search-pagination">
            <SearchPagination index={index} />
          </div>
        </>
      )}
    </div>
  );
};

export default SearchHitContainer;
