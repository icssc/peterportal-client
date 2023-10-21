import React, { useState, useEffect, Component, FC } from "react";
import "./SearchModule.scss";
import wfs from "websoc-fuzzy-search";
import axios from "axios";
import Form from "react-bootstrap/Form";
import InputGroup from "react-bootstrap/InputGroup";
import { Search } from "react-bootstrap-icons";

import { useAppDispatch, useAppSelector } from "../../store/hooks";
import {
  setNames,
  setPageNumber,
  setResults,
} from "../../store/slices/searchSlice";
import { searchAPIResults } from "../../helpers/util";
import {
  SearchIndex,
  BatchCourseData,
  CourseGQLResponse,
  ProfessorGQLResponse,
  BatchProfessorData,
} from "../../types/types";
import { NUM_RESULTS_PER_PAGE  } from "src/helpers/constants";

const SEARCH_TIMEOUT_MS = 500;
const FULL_RESULT_THRESHOLD = 3;
const INITIAL_MAX_PAGE = 5;

interface SearchModuleProps {
  index: SearchIndex;
}

const SearchModule: FC<SearchModuleProps> = ({ index }) => {
  const dispatch = useAppDispatch();
  const courseSearch = useAppSelector((state) => state.search.courses);
  const professorSearch = useAppSelector((state) => state.search.professors);
  const [hasFullResults, setHasFullResults] = useState(false);
  const [lastQuery, setLastQuery] = useState("");
  let pendingRequest: NodeJS.Timeout | null = null;

  // Search empty string to load some results
  useEffect(() => {
    searchNames("");
  }, []);

  // Refresh search results when names and page number changes
  useEffect(() => {
    searchResults("courses", courseSearch.pageNumber, courseSearch.names);
  }, [courseSearch.names, courseSearch.pageNumber]);
  useEffect(() => {
    searchResults(
      "professors",
      professorSearch.pageNumber,
      professorSearch.names
    );
  }, [professorSearch.names, professorSearch.pageNumber]);

  let searchNames = (query: string) => {
    try {
      // Get all results only when query changes or user reaches the fourth page or after
      const currentPage =
        index === "courses"
          ? courseSearch.pageNumber
          : professorSearch.pageNumber;
      let nameResults = wfs({
        query: query,
        resultType: index === "courses" ? "COURSE" : "INSTRUCTOR",
        // Load INITIAL_MAX_PAGE pages first
        // when user reaches the 4th page or after, load all results
        numResults:
          lastQuery !== query || currentPage < FULL_RESULT_THRESHOLD
            ? NUM_RESULTS_PER_PAGE  * INITIAL_MAX_PAGE
            : undefined,
      });
      let names: string[] = [];
      if (index == "courses") {
        names = Object.keys(nameResults);
      } else if (index == "professors") {
        names = Object.keys(nameResults).map(
          (n) => nameResults[n].metadata.ucinetid
        ) as string[];
      }
      console.log("From frontend search", names);
      dispatch(setNames({ index, names }));
      // reset page number and hasFullResults flag if query changes
      if (query !== lastQuery) {
        dispatch(setPageNumber({ index, pageNumber: 0 }));
        setHasFullResults(false);
        setLastQuery(query);
      }
    } catch (e) {
      console.log(e);
    }
  };

  let searchResults = async (
    index: SearchIndex,
    pageNumber: number,
    names: string[]
  ) => {
    if (!hasFullResults && pageNumber >= FULL_RESULT_THRESHOLD) {
      setHasFullResults(true);
      searchNames(lastQuery);
    }
    // Get the subset of names based on the page
    let pageNames = names.slice(
      NUM_RESULTS_PER_PAGE  * pageNumber,
      NUM_RESULTS_PER_PAGE  * (pageNumber + 1)
    );
    let results = await searchAPIResults(index, pageNames);
    dispatch(setResults({ index, results: Object.values(results) }));
  };

  let searchNamesAfterTimeout = (query: string) => {
    if (pendingRequest) {
      clearTimeout(pendingRequest);
    }
    let timeout = setTimeout(() => {
      searchNames(query);
      pendingRequest = null;
    }, SEARCH_TIMEOUT_MS);
    pendingRequest = timeout;
  };

  let coursePlaceholder = "Search a course number or department";
  let professorPlaceholder = "Search a professor";
  let placeholder =
    index == "courses" ? coursePlaceholder : professorPlaceholder;

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
