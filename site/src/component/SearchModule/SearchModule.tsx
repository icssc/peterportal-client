import { useState, useEffect, FC, useCallback } from 'react';
import './SearchModule.scss';
import Form from 'react-bootstrap/Form';
import InputGroup from 'react-bootstrap/InputGroup';
import { Search } from 'react-bootstrap-icons';

import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setQuery, setPageNumber, setResults } from '../../store/slices/searchSlice';
import { transformGQLData } from '../../helpers/util';
import { CourseGQLData, CourseGQLResponse, ProfessorGQLData, SearchIndex } from '../../types/types';
import { NUM_RESULTS_PER_PAGE } from '../../helpers/constants';
import axios from 'axios';

const SEARCH_TIMEOUT_MS = 300;

interface SearchModuleProps {
  index: SearchIndex;
}

const SearchModule: FC<SearchModuleProps> = ({ index }) => {
  const dispatch = useAppDispatch();
  const search = useAppSelector((state) => state.search[index]);
  const [pendingRequest, setPendingRequest] = useState<NodeJS.Timeout | null>(null);
  // const [prevIndex, setPrevIndex] = useState<SearchIndex | null>(null);

  // // Search empty string to load some results on intial visit/when switching between courses and professors tabs
  // // make sure this runs before everything else for best performance and avoiding bugs
  // if (index !== prevIndex) {
  //   setPrevIndex(index);
  //   fuzzySearch('');
  // }

  // clear results and reset page number when component unmounts
  // results will persist otherwise, e.g. current page of results from catalogue carries over to roadmap search container
  useEffect(() => {
    return () => {
      dispatch(setPageNumber({ index: 'courses', pageNumber: 0 }));
      dispatch(setPageNumber({ index: 'professors', pageNumber: 0 }));
      dispatch(setResults({ index: 'courses', results: [], count: 0 }));
      dispatch(setResults({ index: 'professors', results: [], count: 0 }));
    };
  }, [dispatch]);

  interface SearchResults {
    count: number;
    results: CourseGQLResponse[];
  }

  const fuzzySearch = useCallback(
    async (query: string) => {
      const params = new URLSearchParams({
        query,
        limit: `${NUM_RESULTS_PER_PAGE}`,
        offset: `${NUM_RESULTS_PER_PAGE * search.pageNumber}`,
        resultType: index === 'courses' ? 'course' : 'instructor',
      });
      const { count, results } = (
        await axios.get<{ payload: SearchResults }>(
          `https://staging-141.api-next.peterportal.org/v1/rest/search?${params}`,
        )
      ).data.payload;
      console.log(count, results);
      const transformedData = results.map((result) => transformGQLData(index, result)) as
        | CourseGQLData[]
        | ProfessorGQLData[];
      console.log(transformedData);
      dispatch(setResults({ index, results: transformedData, count }));
    },
    [dispatch, index, search.pageNumber],
  );

  // Refresh search results when query and/or page number changes
  useEffect(() => {
    fuzzySearch(search.query);
  }, [search.query, fuzzySearch]);

  const searchNamesAfterTimeout = (query: string) => {
    if (pendingRequest) {
      clearTimeout(pendingRequest);
    }
    const timeout = setTimeout(() => {
      dispatch(setQuery({ index, query }));
      setPendingRequest(null);
    }, SEARCH_TIMEOUT_MS);
    setPendingRequest(timeout);
  };

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
            aria-label="search"
            type="text"
            placeholder={placeholder}
            onChange={(e) => searchNamesAfterTimeout(e.target.value)}
            defaultValue={search.query}
          />
        </InputGroup>
      </Form.Group>
    </div>
  );
};

export default SearchModule;
