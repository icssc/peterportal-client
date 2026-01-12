import { useEffect, FC, useRef } from 'react';
import './SearchHitContainer.scss';

import { useAppSelector } from '../../store/hooks';

import { CourseGQLData, ProfessorGQLData } from '../../types/types';
import SearchPagination from '../SearchPagination/SearchPagination';
import NoResults from '../NoResults/NoResults';
import LoadingSpinner from '../LoadingSpinner/LoadingSpinner';
import CourseHitItem from '../../app/search/CourseHitItem';
import ProfessorHitItem from '../../app/search/ProfessorHitItem';

const SearchResults: FC = () => {
  const viewIndex = useAppSelector((state) => state.search.viewIndex);
  const results = useAppSelector((state) => state.search[viewIndex].results);

  if (viewIndex === 'courses') {
    return (results as CourseGQLData[]).map((course) => {
      return <CourseHitItem key={course.id} {...course} />;
    });
  } else {
    return (results as ProfessorGQLData[]).map((professor) => (
      <ProfessorHitItem key={professor.ucinetid} {...professor} />
    ));
  }
};

const SearchHitContainer: FC = () => {
  const viewIndex = useAppSelector((state) => state.search.viewIndex);
  const { query, results } = useAppSelector((state) => state.search[viewIndex]);
  const searchInProgress = useAppSelector((state) => state.search.inProgressSearchOperation !== 'none');
  const containerDivRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    containerDivRef.current!.scrollTop = 0;
  }, [results]);

  return (
    <div ref={containerDivRef} className="search-hit-container">
      {searchInProgress && <LoadingSpinner />}
      {!searchInProgress && (!query || results.length === 0) && (
        <NoResults
          showPrompt={query === ''}
          prompt={`Start typing in the search bar to search for courses or instructors...`}
        />
      )}
      {!searchInProgress && query && results.length > 0 && (
        <>
          <SearchResults />
          <SearchPagination />
        </>
      )}
    </div>
  );
};

export default SearchHitContainer;
