import { useState, useEffect, FC, useCallback } from 'react';
import './SearchModule.scss';
import wfs from 'websoc-fuzzy-search';
import Form from 'react-bootstrap/Form';
import InputGroup from 'react-bootstrap/InputGroup';
import { Search } from 'react-bootstrap-icons';

import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setHasFullResults, setLastQuery, setNames, setPageNumber, setResults } from '../../store/slices/searchSlice';
import { searchAPIResults } from '../../helpers/util';
import { SearchIndex } from '../../types/types';
import { NUM_RESULTS_PER_PAGE } from '../../helpers/constants';

const SEARCH_TIMEOUT_MS = 500;
const FULL_RESULT_THRESHOLD = 3;
const INITIAL_MAX_PAGE = 5;

interface SearchModuleProps {
  index: SearchIndex;
}

const SearchModule: FC<SearchModuleProps> = ({ index }) => {
  const dispatch = useAppDispatch();
  const search = useAppSelector((state) => state.search[index]);
  const [pendingRequest, setPendingRequest] = useState<NodeJS.Timeout | null>(null);
  const [prevIndex, setPrevIndex] = useState<SearchIndex | null>(null);

  const searchNames = useCallback(
    (query: string) => {
      // Get all results only when query changes or user reaches the fourth page or after
      const nameResults = wfs({
        query: query,
        resultType: index === 'courses' ? 'COURSE' : 'INSTRUCTOR',
        // Load INITIAL_MAX_PAGE pages first
        // when user reaches the 4th page or after, load all results
        numResults:
          search.lastQuery !== query || search.pageNumber < FULL_RESULT_THRESHOLD
            ? NUM_RESULTS_PER_PAGE * INITIAL_MAX_PAGE
            : undefined,
      });
      let names: string[] = [];
      if (index === 'courses') {
        names = Object.keys(nameResults ?? {});
      } else if (index === 'professors') {
        names = Object.keys(nameResults ?? {}).map(
          (n) =>
            (
              (nameResults ?? {})[n].metadata as {
                ucinetid: string;
              }
            ).ucinetid,
        ) as string[];
      }
      console.log('From frontend search', names);
      dispatch(setNames({ index, names }));
      // reset page number and hasFullResults flag if query changes
      if (query !== search.lastQuery) {
        dispatch(setPageNumber({ index, pageNumber: 0 }));
        dispatch(setHasFullResults({ index, hasFullResults: false }));
        dispatch(setLastQuery({ index, lastQuery: query }));
      }
    },
    [dispatch, search.pageNumber, search.lastQuery, index],
  );

  const searchResults = useCallback(async () => {
    if (search.names.length === 0) {
      dispatch(setResults({ index, results: [] }));
      return;
    }
    if (!search.hasFullResults && search.pageNumber >= FULL_RESULT_THRESHOLD) {
      dispatch(setHasFullResults({ index, hasFullResults: true }));
      searchNames(search.lastQuery);
      return;
    }
    // Get the subset of names based on the page
    const pageNames = search.names.slice(
      NUM_RESULTS_PER_PAGE * search.pageNumber,
      NUM_RESULTS_PER_PAGE * (search.pageNumber + 1),
    );
    const results = await searchAPIResults(index, pageNames);
    dispatch(setResults({ index, results: Object.values(results) }));
  }, [dispatch, search.names, search.pageNumber, index, search.hasFullResults, search.lastQuery, searchNames]);

  // Refresh search results when names and page number changes (controlled by searchResults dependency array)
  useEffect(() => {
    searchResults();
  }, [index, searchResults]);

  // reset page number and clear results on unmount
  // results will persist otherwise, e.g. current page of results from catalogue carries over to roadmap search container
  useEffect(() => {
    return () => {
      dispatch(setPageNumber({ index: 'courses', pageNumber: 0 }));
      dispatch(setPageNumber({ index: 'professors', pageNumber: 0 }));
      dispatch(setResults({ index: 'courses', results: [] }));
      dispatch(setResults({ index: 'professors', results: [] }));
    };
  }, [dispatch]);

  const searchNamesAfterTimeout = (query: string) => {
    if (pendingRequest) {
      clearTimeout(pendingRequest);
    }
    const timeout = setTimeout(() => {
      searchNames(query);
      setPendingRequest(null);
    }, SEARCH_TIMEOUT_MS);
    setPendingRequest(timeout);
  };

  // Search empty string to load some results
  if (index !== prevIndex) {
    searchNames('');
    setPrevIndex(index);
  }

  const coursePlaceholder = 'Search a course number or department';
  const professorPlaceholder = 'Search a professor';
  const placeholder = index === 'courses' ? coursePlaceholder : professorPlaceholder;

  return (
    <div className="search-module">
      <Form.Group className="mb-3">
        <InputGroup>
          <InputGroup.Prepend>
            <InputGroup.Text>
              <Search />
            </InputGroup.Text>
          </InputGroup.Prepend>
          <Form.Control
            className="search-bar"
            type="text"
            placeholder={placeholder}
            onChange={(e) => searchNamesAfterTimeout(e.target.value)}
          />
        </InputGroup>
      </Form.Group>
    </div>
  );
};

export default SearchModule;
