import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import type { RootState } from '../store';
import { CourseGQLData, ProfessorGQLData } from '../../types/types';

// Define a type for the slice state
interface PopupState {
  course: CourseGQLData;
  professor: ProfessorGQLData;
}

// Define the initial state using that type
const initialState: PopupState = {
  course: null!,
  professor: null!,
};

export const reviewSlice = createSlice({
  name: 'popup',
  // `createSlice` will infer the state type from the `initialState` argument
  initialState,
  reducers: {
    // Use the PayloadAction type to declare the contents of `action.payload`
    setCourse: (state, action: PayloadAction<CourseGQLData>) => {
      state.course = action.payload;
    },
    setProfessor: (state, action: PayloadAction<ProfessorGQLData>) => {
      state.professor = action.payload;
    },
  },
});

export const { setCourse, setProfessor } = reviewSlice.actions;

// Other code such as selectors can use the imported `RootState` type
export const selectCourse = (state: RootState) => state.popup.course;
export const selectProfessor = (state: RootState) => state.popup.professor;

export default reviewSlice.reducer;
