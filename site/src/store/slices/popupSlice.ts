import { createSlice, PayloadAction } from '@reduxjs/toolkit'
import type { RootState } from '../store'
import { CourseData, ProfessorData } from '../../types/types';

// Define a type for the slice state
interface PopupState {
    course: CourseData;
    professor: ProfessorData;
}

// Define the initial state using that type
const initialState: PopupState = {
    course: null!,
    professor: null!
}

export const reviewSlice = createSlice({
    name: 'popup',
    // `createSlice` will infer the state type from the `initialState` argument
    initialState,
    reducers: {
        // Use the PayloadAction type to declare the contents of `action.payload`
        setCourse: (state, action: PayloadAction<CourseData>) => {
            state.course = action.payload;
        },
        setProfessor: (state, action: PayloadAction<ProfessorData>) => {
            state.professor = action.payload;
        }
    },
})

export const { setCourse, setProfessor } = reviewSlice.actions

// Other code such as selectors can use the imported `RootState` type
export const selectCourse = (state: RootState) => state.popup.course;
export const selectProfessor = (state: RootState) => state.popup.professor;

export default reviewSlice.reducer