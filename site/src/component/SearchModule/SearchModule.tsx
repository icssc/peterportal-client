import { useState, useEffect, FC, useRef } from 'react';
import './SearchModule.scss';

import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { SearchIndex, SearchResultData } from '../../types/types';
import { stringifySearchFilters } from '../../helpers/searchFilters.ts';
import { NUM_RESULTS_PER_PAGE } from '../../helpers/constants';
import { setShowSavedCourses } from '../../store/slices/roadmapSlice';
import trpc from '../../trpc.ts';
import { selectCourseFilters, setQuery, setResults } from '../../store/slices/searchSlice';
import { transformGQLData } from '../../helpers/util';

import { InputAdornment, IconButton, TextField } from '@mui/material';
import SearchIcon from '@mui/icons-material/Search';
import SearchFilters from '../SearchFilters/SearchFilters.tsx';

const SEARCH_TIMEOUT_MS = 300;

interface SearchModuleProps {
  index: SearchIndex;
}

const SearchModule: FC<SearchModuleProps> = ({ index }) => {
  const dispatch = useAppDispatch();
  const search = useAppSelector((state) => state.search[index]);
  const filterOptions = useAppSelector(selectCourseFilters);
  const [searchQuery, setSearchQuery] = useState('');
  const [pendingRequest, setPendingRequest] = useState<number | null>(null);
  const abortControllerRef = useRef<AbortController | null>(null);

  // fuzzySearch is defined after filter state so it can depend on them safely

  // Cleanup abort controller on unmount
  useEffect(() => {
    const controller = abortControllerRef.current;
    return () => {
      controller?.abort();
    };
  }, []);

  const searchImmediately = (query: string) => {
    if (pendingRequest) clearTimeout(pendingRequest);
    if (location.pathname === '/') {
      dispatch(setShowSavedCourses(!query));
    }
    if (query && query !== search.query) {
      dispatch(setQuery({ index, query }));
      setPendingRequest(null);
    }
  };

  const searchAfterTimeout = (query: string) => {
    setSearchQuery(query);
    if (pendingRequest) clearTimeout(pendingRequest);
    const timeout = window.setTimeout(() => searchImmediately(query), SEARCH_TIMEOUT_MS);
    setPendingRequest(timeout);
  };

  const coursePlaceholder = 'Search for a course...';
  const professorPlaceholder = 'Search a professor';
  const placeholder = index === 'courses' ? coursePlaceholder : professorPlaceholder;

  // Run search when query, page, or filters change
  useEffect(() => {
    // Cancel any in-flight request
    abortControllerRef.current?.abort();
    const abortController = new AbortController();
    abortControllerRef.current = abortController;

    const fuzzySearch = async () => {
      try {
        const { stringifiedLevels, stringifiedGeCategories, stringifiedDepartments } =
          stringifySearchFilters(filterOptions);

        const base = {
          query: search.query,
          take: NUM_RESULTS_PER_PAGE,
          skip: NUM_RESULTS_PER_PAGE * search.pageNumber,
          resultType: index === 'courses' ? 'course' : 'instructor',
        } as const;

        const payload = {
          ...base,
          ...(index === 'courses' && stringifiedDepartments ? { department: stringifiedDepartments } : {}),
          ...(index === 'courses' && stringifiedLevels ? { courseLevel: stringifiedLevels } : {}),
          ...(index === 'courses' && stringifiedGeCategories ? { ge: stringifiedGeCategories } : {}),
        } as Parameters<typeof trpc.search.get.query>[0];

        const { count, results } = await trpc.search.get.query(payload, { signal: abortController.signal });

        if (!abortController.signal.aborted) {
          dispatch(
            setResults({
              index,
              results: results.map((x) => transformGQLData(index, x.result)) as SearchResultData,
              count,
            }),
          );
        }
      } catch (error) {
        if (error instanceof Error && error.name !== 'AbortError') {
          console.error('Search error:', error);
        }
      }
    };

    fuzzySearch();

    // Re-run when query/page or any selected filter changes
  }, [dispatch, index, search.pageNumber, search.query, filterOptions]);

  const endAdornment = (
    <InputAdornment position="end">
      <IconButton aria-label="Search" onClick={() => searchImmediately(searchQuery)}>
        <SearchIcon />
      </IconButton>
    </InputAdornment>
  );

  return (
    <div className="search-module">
      <TextField
        variant="outlined"
        className="search-bar"
        aria-label="search"
        type="text"
        placeholder={placeholder}
        onChange={(e) => searchAfterTimeout(e.target.value)}
        defaultValue={search.query}
        autoCorrect="off"
        slotProps={{ input: { endAdornment, className: 'input-wrapper' } }}
      />
      {index === 'courses' && <SearchFilters />}
    </div>
  );
};

export default SearchModule;
