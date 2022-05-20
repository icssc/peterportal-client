import { createSlice, PayloadAction } from '@reduxjs/toolkit'
import type { RootState } from '../store'
import { CourseGQLData, ProfessorGQLData, SearchIndex } from '../../types/types';

interface SearchData {
    names: string[];
    pageNumber: number;
    results: CourseGQLData[] | ProfessorGQLData[];
}

// Define a type for the slice state
interface SearchState {
    courses: SearchData;
    professors: SearchData;
}

// Define the initial state using that type
const initialState: SearchState = {
    courses: {
        names: [],
        pageNumber: 0,
        results: []
    },
    professors: {
        names: [],
        pageNumber: 0,
        results: []
    },
}

export const searchSlice = createSlice({
    name: 'search',
    // `createSlice` will infer the state type from the `initialState` argument
    initialState,
    reducers: {
        // Use the PayloadAction type to declare the contents of `action.payload`
        setNames: (state, action: PayloadAction<{index: SearchIndex, names: SearchData['names']}>) => {
            state[action.payload.index].names = action.payload.names;
        },
        setPageNumber: (state, action: PayloadAction<{index: SearchIndex, pageNumber: SearchData['pageNumber']}>) => {
            state[action.payload.index].pageNumber = action.payload.pageNumber;
        },
        setResults: (state, action: PayloadAction<{index: SearchIndex, results: SearchData['results']}>) => {
            state[action.payload.index].results = action.payload.results;
        }
    },
})

export const { setNames, setPageNumber, setResults } = searchSlice.actions

export default searchSlice.reducer