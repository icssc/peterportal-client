import { useEffect, FC, useRef } from 'react';
import './SearchHitContainer.scss';

import { useAppSelector } from '../../store/hooks';

import { SearchIndex, CourseGQLData, ProfessorGQLData, SearchResultData } from '../../types/types';
import SearchPagination from '../SearchPagination/SearchPagination';
import NoResults from '../NoResults/NoResults';
import LoadingSpinner from '../LoadingSpinner/LoadingSpinner';
import CourseHitItem from '../../app/search/CourseHitItem';
import ProfessorHitItem from '../../app/search/ProfessorHitItem';

// TODO: CourseHitItem and ProfessorHitem should not need index
// investigate: see if you can refactor respective components to use course id/ucinetid for keys instead then remove index from props
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
  const { query, results, searchInProgress } = useAppSelector((state) => state.search[index]);
  const containerDivRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    containerDivRef.current!.scrollTop = 0;
  }, [results]);

  return (
    <div ref={containerDivRef} className="search-hit-container">
      {searchInProgress && <LoadingSpinner />}
      {!searchInProgress && results.length === 0 && (
        <NoResults showPrompt={query === ''} prompt={`Start typing in the search bar to search for ${index}...`} />
      )}
      {!searchInProgress && results.length > 0 && (
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
