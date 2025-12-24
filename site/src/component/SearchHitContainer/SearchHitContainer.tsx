import { useEffect, FC, useRef } from 'react';
import './SearchHitContainer.scss';
import InfiniteScroll from 'react-infinite-scroll-component';

import { setPageNumber } from '../../store/slices/searchSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { CourseGQLData, ProfessorGQLData } from '../../types/types';

import NoResults from '../NoResults/NoResults';
import LoadingSpinner from '../LoadingSpinner/LoadingSpinner';
import CourseHitItem from '../../app/search/CourseHitItem';
import ProfessorHitItem from '../../app/search/ProfessorHitItem';

const SearchResults: FC = () => {
  const dispatch = useAppDispatch();
  const viewIndex = useAppSelector((state) => state.search.viewIndex);
  const { results, pageNumber } = useAppSelector((state) => state.search[viewIndex]);

  const updatePageNumber = () => {
    dispatch(setPageNumber(pageNumber + 1));
  };

  return (
    <InfiniteScroll
      dataLength={results.length}
      next={updatePageNumber}
      hasMore={true} // charlie @todo update this to not always be true
      loader={<LoadingSpinner />}
      scrollableTarget="mobileScrollContainer"
    >
      {viewIndex === 'courses'
        ? (results as CourseGQLData[]).map((course) => <CourseHitItem key={course.id} {...course} />)
        : (results as ProfessorGQLData[]).map((professor) => (
            <ProfessorHitItem key={professor.ucinetid} {...professor} />
          ))}
    </InfiniteScroll>
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
      {query && results.length > 0 && <SearchResults />}
    </div>
  );
};

export default SearchHitContainer;
