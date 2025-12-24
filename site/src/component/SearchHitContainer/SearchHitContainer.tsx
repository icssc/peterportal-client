import { useEffect, FC, useRef } from 'react';
import './SearchHitContainer.scss';
import InfiniteScroll from 'react-infinite-scroll-component';

import { setPageNumber } from '../../store/slices/searchSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { useSearchTrigger } from '../../hooks/search';
import { CourseGQLData, ProfessorGQLData } from '../../types/types';

// import SearchPagination from '../SearchPagination/SearchPagination';
import NoResults from '../NoResults/NoResults';
import LoadingSpinner from '../LoadingSpinner/LoadingSpinner';
import CourseHitItem from '../../app/search/CourseHitItem';
import ProfessorHitItem from '../../app/search/ProfessorHitItem';

// charlie @todo make sure this works w/ infinite scroll

const SearchResults: FC = () => {
  const dispatch = useAppDispatch();
  const viewIndex = useAppSelector((state) => state.search.viewIndex);
  const results = useAppSelector((state) => state.search[viewIndex].results);
  const pageNumber = useAppSelector((state) => state.search[viewIndex].pageNumber);

  const useFetchMore = () => {
    dispatch(setPageNumber(pageNumber + 1));
    useSearchTrigger();
  };

  return (
    <>
      <p>Number of results: {results.length}</p>
      <InfiniteScroll
        dataLength={results.length}
        next={useFetchMore}
        hasMore={true} // charlie @todo update this to not always be true
        loader={<LoadingSpinner />}
        scrollableTarget="scrollContainer"
      >
        {viewIndex === 'courses'
          ? (results as CourseGQLData[]).map((course) => <CourseHitItem key={course.id} {...course} />)
          : (results as ProfessorGQLData[]).map((professor) => (
              <ProfessorHitItem key={professor.ucinetid} {...professor} />
            ))}
      </InfiniteScroll>
    </>
  );
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
      {searchInProgress && results.length === 0 && <LoadingSpinner />}
      {!searchInProgress && (!query || results.length === 0) && (
        <NoResults
          showPrompt={query === ''}
          prompt={`Start typing in the search bar to search for courses or instructors...`}
        />
      )}
      {query && results.length > 0 && (
        <>
          <SearchResults />
          {/* <SearchPagination /> */}
        </>
      )}
    </div>
  );
};

export default SearchHitContainer;
