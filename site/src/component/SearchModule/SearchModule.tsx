import { FC, useEffect } from 'react';
import { Search } from 'react-bootstrap-icons';
import Form from 'react-bootstrap/Form';
import InputGroup from 'react-bootstrap/InputGroup';
import wfs from 'websoc-fuzzy-search';
import './SearchModule.scss';

import { searchAPIResults } from '../../helpers/util';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setNames, setResults } from '../../store/slices/searchSlice';
import { SearchIndex } from '../../types/types';

const PAGE_SIZE = 10;
const SEARCH_TIMEOUT_MS = 500;

interface SearchModuleProps {
  index: SearchIndex;
}

const SearchModule: FC<SearchModuleProps> = ({ index }) => {
  const dispatch = useAppDispatch();
  const courseSearch = useAppSelector((state) => state.search.courses);
  const professorSearch = useAppSelector((state) => state.search.professors);
  let pendingRequest: NodeJS.Timeout | null = null;

  // Search empty string to load some results
  useEffect(() => {
    searchNames('');
  }, [index]);

  // Refresh search results when names and page number changes
  useEffect(() => {
    searchResults('courses', courseSearch.pageNumber, courseSearch.names);
  }, [courseSearch.names, courseSearch.pageNumber]);
  useEffect(() => {
    searchResults('professors', professorSearch.pageNumber, professorSearch.names);
  }, [professorSearch.names, professorSearch.pageNumber]);

  const searchNames = (query: string) => {
    try {
      /*
                TODO: Search optimization
                - Currently sending a query request for every input change
                - Goal is to have only one query request pending
                - Use setTimeout/clearTimeout to keep track of pending query request
            */
      const nameResults = wfs({
        query: query,
        numResults: PAGE_SIZE * 5,
        resultType: index === 'courses' ? 'COURSE' : 'INSTRUCTOR',
        filterOptions: {},
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
    } catch (e) {
      console.log(e);
    }
  };

  const searchResults = async (index: SearchIndex, pageNumber: number, names: string[]) => {
    // Get the subset of names based on the page
    const pageNames = names.slice(PAGE_SIZE * pageNumber, PAGE_SIZE * (pageNumber + 1));
    const results = await searchAPIResults(index, pageNames);
    dispatch(setResults({ index, results: Object.values(results) }));
  };

  const searchNamesAfterTimeout = (query: string) => {
    if (pendingRequest) {
      clearTimeout(pendingRequest);
    }
    const timeout = setTimeout(() => {
      searchNames(query);
      pendingRequest = null;
    }, SEARCH_TIMEOUT_MS);
    pendingRequest = timeout;
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
          />
        </InputGroup>
      </Form.Group>
    </div>
  );
};

export default SearchModule;
