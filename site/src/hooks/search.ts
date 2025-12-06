import { useEffect, useRef } from 'react';
import { NUM_RESULTS_PER_PAGE } from '../helpers/constants';
import { FilterOptions, stringifySearchFilters } from '../helpers/searchFilters';
import { useAppDispatch, useAppSelector } from '../store/hooks';
import { selectCourseFilters, setResults, setSearchStarted } from '../store/slices/searchSlice';
import trpc from '../trpc';
import { SearchIndex, SearchResultData } from '../types/types';
import { transformGQLData } from '../helpers/util';

async function performSearch(
  index: SearchIndex,
  query: string,
  page: number,
  filters: FilterOptions,
  signal: AbortSignal,
) {
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

  return await trpc.search.get.query(payload, { signal });
}

/**
 * automatically initiates a new search and updates the results slice whenever the search query
 * or filters change
 */
export function useSearchTrigger(index: SearchIndex) {
  const searchState = useAppSelector((state) => state.search[index]);
  const filterOptions = useAppSelector(selectCourseFilters);
  const abortControllerRef = useRef<AbortController | null>(null);
  const dispatch = useAppDispatch();

  useEffect(() => {
    const controller = abortControllerRef.current;
    return () => controller?.abort();
  }, []);

  useEffect(() => {
    abortControllerRef.current?.abort();
    const abortController = new AbortController();
    abortControllerRef.current = abortController;

    if (!searchState.query) return;
    dispatch(setSearchStarted());

    performSearch(index, searchState.query, searchState.pageNumber, filterOptions, abortController.signal)
      .then(({ count, results }) => {
        if (abortController.signal.aborted) return;
        const transformedResults = results.map((x) => transformGQLData(index, x.result)) as SearchResultData;
        dispatch(setResults({ index, results: transformedResults, count }));
      })
      .catch((error) => {
        if (error instanceof Error && error.name !== 'AbortError') console.error('Search error:', error);
      });
  }, [dispatch, index, searchState.query, searchState.pageNumber, filterOptions]);
}
