import { createSelector, createSlice, PayloadAction } from '@reduxjs/toolkit';
import { SearchIndex, SearchResultData } from '../../types/types';
import { FilterOptions } from '../../helpers/searchFilters';
import { RootState } from '../store';

interface SearchData {
  query: string;
  lastQuery: string;
  pageNumber: number;
  results: SearchResultData;
  count: number;
  searchInProgress: boolean;
}

export const searchSlice = createSlice({
  name: 'search',
  initialState: {
    courses: {
      query: '',
      lastQuery: '',
      pageNumber: 0,
      results: [],
      count: 0,
      searchInProgress: false,
    } as SearchData,
    courseLevels: [] as string[],
    courseGeCategories: [] as string[],
    courseDepartments: [] as string[],
    professors: {
      query: '',
      lastQuery: '',
      pageNumber: 0,
      results: [],
      count: 0,
      searchInProgress: false,
    } as SearchData,
  },
  reducers: {
    // Use the PayloadAction type to declare the contents of `action.payload`
    setQuery: (state, action: PayloadAction<{ index: SearchIndex; query: SearchData['query'] }>) => {
      state[action.payload.index].query = action.payload.query;
      state[action.payload.index].searchInProgress = true;
    },
    setPageNumber: (state, action: PayloadAction<{ index: SearchIndex; pageNumber: SearchData['pageNumber'] }>) => {
      state[action.payload.index].pageNumber = action.payload.pageNumber;
    },
    setResults: (
      state,
      action: PayloadAction<{ index: SearchIndex; results: SearchData['results']; count: SearchData['count'] }>,
    ) => {
      state[action.payload.index].searchInProgress = false;
      state[action.payload.index].results = action.payload.results;
      state[action.payload.index].count = action.payload.count;
      if (state[action.payload.index].lastQuery !== state[action.payload.index].query) {
        state[action.payload.index].pageNumber = 0;
        state[action.payload.index].lastQuery = state[action.payload.index].query;
      }
    },
    setCourseFilters: (state, action: PayloadAction<FilterOptions>) => {
      const { departments, geCategories, levels } = action.payload;
      state.courseDepartments = departments;
      state.courseGeCategories = geCategories;
      state.courseLevels = levels;
    },
  },
});

export const selectCourseFilters = createSelector(
  (state: RootState) => state.search.courseDepartments,
  (state: RootState) => state.search.courseGeCategories,
  (state: RootState) => state.search.courseLevels,
  (departments, geCategories, levels) => ({ departments, geCategories, levels }),
);

export const { setQuery, setPageNumber, setResults, setCourseFilters } = searchSlice.actions;

export default searchSlice.reducer;
