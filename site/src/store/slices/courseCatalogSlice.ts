import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { CourseGQLData } from '../../types/types';
import type { RootState } from '../store';

const initialState: { courses: Record<string, CourseGQLData> } = { courses: {} };

export const courseCatalogSlice = createSlice({
  name: 'courseCatalog',
  initialState,
  reducers: {
    setCourse(state, action: PayloadAction<{ courseId: string; data: CourseGQLData }>) {
      state.courses[action.payload.courseId] = action.payload.data;
    },
  },
});
3;
export const { setCourse } = courseCatalogSlice.actions;

export const selectCourseCatalog = (state: RootState) => state.courseCatalog.courses;

export default courseCatalogSlice.reducer;
