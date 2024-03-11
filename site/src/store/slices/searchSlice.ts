import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { CourseGQLData, ProfessorGQLData, SearchIndex } from '../../types/types';

interface SearchData {
  query: string;
  pageNumber: number;
  results: CourseGQLData[] | ProfessorGQLData[];
  count: number;
}

// Define a type for the slice state
interface SearchState {
  courses: SearchData;
  professors: SearchData;
}

// Define the initial state using that type
const initialState: SearchState = {
  courses: {
    query: '',
    pageNumber: 0,
    results: [],
    count: 0,
  },
  professors: {
    query: '',
    pageNumber: 0,
    results: [],
    count: 0,
  },
};

export const searchSlice = createSlice({
  name: 'search',
  // `createSlice` will infer the state type from the `initialState` argument
  initialState,
  reducers: {
    // Use the PayloadAction type to declare the contents of `action.payload`
    setQuery: (state, action: PayloadAction<{ index: SearchIndex; query: SearchData['query'] }>) => {
      state[action.payload.index].query = action.payload.query;
    },
    setPageNumber: (state, action: PayloadAction<{ index: SearchIndex; pageNumber: SearchData['pageNumber'] }>) => {
      state[action.payload.index].pageNumber = action.payload.pageNumber;
    },
    setResults: (
      state,
      action: PayloadAction<{ index: SearchIndex; results: SearchData['results']; count: SearchData['count'] }>,
    ) => {
      state[action.payload.index].results = action.payload.results;
      state[action.payload.index].count = action.payload.count;
      state[action.payload.index].pageNumber = 0;
    },
  },
});

export const { setQuery, setPageNumber, setResults } = searchSlice.actions;

export default searchSlice.reducer;
