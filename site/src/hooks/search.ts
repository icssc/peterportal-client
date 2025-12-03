import { useCallback, useEffect, useRef } from 'react';
import { NUM_RESULTS_PER_PAGE } from '../helpers/constants';
import { FilterOptions, stringifySearchFilters } from '../helpers/searchFilters';
import { useAppDispatch, useAppSelector } from '../store/hooks';
import {
  selectCourseFilters,
  setFirstPageResults,
  setNewPageResults,
  setSearchViewIndex,
} from '../store/slices/searchSlice';
import trpc from '../trpc';
import { SearchIndex, SearchResultData } from '../types/types';
import { transformGQLData } from '../helpers/util';

type SearchResponseData = { count: number; results: SearchResultData; totalRank: number };
async function performSearch(
  index: SearchIndex,
  query: string,
  page: number,
  filters: FilterOptions,
  signal: AbortSignal,
): Promise<SearchResponseData> {
  const { stringifiedLevels, stringifiedGeCategories, stringifiedDepartments } = stringifySearchFilters(filters);

  const apiCourseFilters = {
    department: stringifiedDepartments,
    courseLevel: stringifiedLevels,
    ge: stringifiedGeCategories,
  };

  const payload = {
    query,
    take: NUM_RESULTS_PER_PAGE,
    skip: NUM_RESULTS_PER_PAGE * page,
    resultType: index === 'courses' ? 'course' : 'instructor',
    ...(index === 'courses' && apiCourseFilters),
  } as const;

  const { count, results } = await trpc.search.get.query(payload, { signal });

  signal.throwIfAborted();

  return {
    count,
    results: results.map((x) => transformGQLData(index, x.result)) as SearchResultData,
    totalRank: results.map((r) => r.rank).reduce((a, b) => a + b, 0),
  };
}

/**
 * automatically initiates a new search and updates the results slice whenever the search query
 * or filters change
 */
export function useSearchTrigger() {
  const inProgressSearch = useAppSelector((state) => state.search.inProgressSearchOperation);
  const visibleSearchIdx = useAppSelector((state) => state.search.viewIndex);

  const searchState = useAppSelector((state) => state.search[visibleSearchIdx]);
  const courseFilters = useAppSelector(selectCourseFilters);
  const showMobileCatalog = useAppSelector((state) => state.roadmap.showMobileCatalog);

  const abortControllerRef = useRef<AbortController | null>(null);
  const dispatch = useAppDispatch();

  useEffect(() => {
    const controller = abortControllerRef.current;
    return () => controller?.abort();
  }, []);

  const regenerateAbortSignal = () => {
    abortControllerRef.current?.abort();
    const abortController = new AbortController();
    abortControllerRef.current = abortController;
    return abortController.signal;
  };

  const handleSearchError = (error: unknown) => {
    if (error instanceof Error && error.name !== 'AbortError') console.error('Search error:', error);
  };

  const handleFirstPageResults = useCallback(
    (index: SearchIndex, data: SearchResponseData) => {
      dispatch(setFirstPageResults({ index, ...data }));
    },
    [dispatch],
  );

  useEffect(() => {
    if (inProgressSearch !== 'newQuery') return;

    const signal = regenerateAbortSignal();

    const searches = [performSearch('courses', searchState.query, 0, courseFilters, signal)];
    if (!showMobileCatalog) {
      const instructorSearch = performSearch('professors', searchState.query, 0, courseFilters, signal);
      searches.push(instructorSearch);
    }

    Promise.all(searches)
      .then(([courseRes, profRes]) => {
        // if a prof search is not triggered, we still want to clear old query results
        profRes ??= { count: 0, results: [], totalRank: 0 };
        handleFirstPageResults('courses', courseRes);
        handleFirstPageResults('professors', profRes);
        const showCoursesFirst = showMobileCatalog || courseRes.totalRank > profRes.totalRank;
        dispatch(setSearchViewIndex(showCoursesFirst ? 'courses' : 'professors'));
      })
      .catch(handleSearchError);
  }, [handleFirstPageResults, inProgressSearch, searchState.query, courseFilters, showMobileCatalog, dispatch]);

  useEffect(() => {
    if (inProgressSearch !== 'newFilters') return;

    performSearch(visibleSearchIdx, searchState.query, 0, courseFilters, regenerateAbortSignal())
      .then((data) => {
        handleFirstPageResults(visibleSearchIdx, data);
      })
      .catch(handleSearchError);
  }, [courseFilters, handleFirstPageResults, inProgressSearch, searchState.query, visibleSearchIdx]);

  useEffect(() => {
    if (inProgressSearch !== 'newPage') return;

    performSearch(visibleSearchIdx, searchState.query, searchState.pageNumber, courseFilters, regenerateAbortSignal())
      .then((data) => {
        dispatch(setNewPageResults({ index: visibleSearchIdx, results: data.results }));
      })
      .catch(handleSearchError);
  }, [courseFilters, dispatch, inProgressSearch, searchState.pageNumber, searchState.query, visibleSearchIdx]);
}
