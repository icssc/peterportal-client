import { useState, useEffect, FC, useCallback, useRef } from 'react';
import './SearchModule.scss';
import Form from 'react-bootstrap/Form';
import InputGroup from 'react-bootstrap/InputGroup';
import { Bag, Search } from 'react-bootstrap-icons';

import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { CourseGQLData, ProfessorGQLData, SearchIndex } from '../../types/types';
import { NUM_RESULTS_PER_PAGE } from '../../helpers/constants';
import { setShowCourseBag } from '../../store/slices/roadmapSlice';
import trpc from '../../trpc.ts';
import { setQuery, setResults } from '../../store/slices/searchSlice';
import { transformGQLData } from '../../helpers/util';

const SEARCH_TIMEOUT_MS = 300;

interface SearchModuleProps {
  index: SearchIndex;
}

const SearchModule: FC<SearchModuleProps> = ({ index }) => {
  const dispatch = useAppDispatch();
  const search = useAppSelector((state) => state.search[index]);
  const showCourseBag = useAppSelector((state) => state.roadmap.showCourseBag);
  const [pendingRequest, setPendingRequest] = useState<number | null>(null);
  const abortControllerRef = useRef<AbortController | null>(null);

  const fuzzySearch = useCallback(
    async (query: string) => {
      if (abortControllerRef.current) {
        abortControllerRef.current.abort();
      }
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

  const searchAfterTimeout = (query: string) => {
    if (pendingRequest) {
      clearTimeout(pendingRequest);
    }
    const timeout = window.setTimeout(() => {
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
      <Form.Group>
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
            onChange={(e) => searchAfterTimeout(e.target.value)}
            defaultValue={search.query}
          />
          {
            // only show course bag icon on roadmap page
            location.pathname === '/roadmap' && (
              <InputGroup.Append>
                <InputGroup.Text onClick={() => dispatch(setShowCourseBag(!showCourseBag))}>
                  <Bag style={{ color: showCourseBag ? 'var(--primary)' : 'var(--text-color)', cursor: 'pointer' }} />
                </InputGroup.Text>
              </InputGroup.Append>
            )
          }
        </InputGroup>
      </Form.Group>
    </div>
  );
};

export default SearchModule;
