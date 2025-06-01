import { useState, useEffect, FC, useCallback, useRef } from 'react';
import './SearchModule.scss';
import Form from 'react-bootstrap/Form';
import InputGroup from 'react-bootstrap/InputGroup';

import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { CourseGQLData, ProfessorGQLData, SearchIndex } from '../../types/types';
import { NUM_RESULTS_PER_PAGE } from '../../helpers/constants';
import { setShowCourseBag } from '../../store/slices/roadmapSlice';
import trpc from '../../trpc.ts';
import { setQuery, setResults } from '../../store/slices/searchSlice';
import { transformGQLData } from '../../helpers/util';

import SearchIcon from '@mui/icons-material/Search';

const SEARCH_TIMEOUT_MS = 300;

interface SearchModuleProps {
  index: SearchIndex;
}

const SearchModule: FC<SearchModuleProps> = ({ index }) => {
  const dispatch = useAppDispatch();
  const search = useAppSelector((state) => state.search[index]);
  const [searchQuery, setSearchQuery] = useState('');
  const [pendingRequest, setPendingRequest] = useState<number | null>(null);
  const abortControllerRef = useRef<AbortController | null>(null);

  const fuzzySearch = useCallback(
    async (query: string) => {
      abortControllerRef.current?.abort();
      const abortController = new AbortController();
      abortControllerRef.current = abortController;
      try {
        const { count, results } = await trpc.search.get.query(
          {
            query,
            take: NUM_RESULTS_PER_PAGE,
            skip: NUM_RESULTS_PER_PAGE * search.pageNumber,
            resultType: index === 'courses' ? 'course' : 'instructor',
          },
          { signal: abortController.signal },
        );
        if (!abortController.signal.aborted) {
          dispatch(
            setResults({
              index,
              results: results.map((x) => transformGQLData(index, x.result)) as CourseGQLData[] | ProfessorGQLData[],
              count,
            }),
          );
        }
      } catch (error) {
        if (error instanceof Error && error.name !== 'AbortError') {
          console.error('Search error:', error);
        }
      }
    },
    [dispatch, index, search.pageNumber],
  );

  // Cleanup abort controller on unmount
  useEffect(() => {
    return () => {
      abortControllerRef.current?.abort();
    };
  }, []);

  // Refresh search results when names and page number changes (controlled by searchResults dependency array)
  useEffect(() => {
    fuzzySearch(search.query);
  }, [search.query, fuzzySearch]);

  const searchImmediately = (query: string) => {
    if (pendingRequest) clearTimeout(pendingRequest);
    if (location.pathname === '/roadmap') {
      dispatch(setShowCourseBag(!query));
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

  return (
    <div className="search-module">
      <Form.Group>
        <InputGroup>
          <Form.Control
            className="search-bar"
            aria-label="search"
            type="search"
            placeholder={placeholder}
            onChange={(e) => searchAfterTimeout(e.target.value)}
            defaultValue={search.query}
            autoCorrect="off"
          />
          <InputGroup.Append>
            <button className="input-group-text" onClick={() => searchImmediately(searchQuery)}>
              <SearchIcon />
            </button>
          </InputGroup.Append>
        </InputGroup>
      </Form.Group>
    </div>
  );
};

export default SearchModule;
