import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { RootState } from '../store';
import type { Coursebag, CourseGQLData } from '../../types/types';
import trpc from '../../trpc';

const initialState: { coursebag: Coursebag } = {
  coursebag: [],
};

export const coursebagSlice = createSlice({
  name: 'coursebag',
  initialState,
  reducers: {
    setCoursebag(state, action: PayloadAction<Coursebag>) {
      state.coursebag = action.payload;
    },
    addCourseToBag: (state, action: PayloadAction<CourseGQLData>) => {
      state.coursebag.push(action.payload);
      localStorage.setItem('coursebag', JSON.stringify(state.coursebag.map((course) => course.id)));
      trpc.savedCourses.add.mutate({ courseId: action.payload.id });
    },
    removeCourseFromBag: (state, action: PayloadAction<CourseGQLData>) => {
      state.coursebag = state.coursebag.filter((course) => course.id !== action.payload.id);
      localStorage.setItem('coursebag', JSON.stringify(state.coursebag.map((course) => course.id)));
      trpc.savedCourses.remove.mutate({ courseId: action.payload.id });
    },
  },
});

export const { setCoursebag, addCourseToBag, removeCourseFromBag } = coursebagSlice.actions;

export const selectUser = (state: RootState) => state.coursebag.coursebag;

export default coursebagSlice.reducer;
