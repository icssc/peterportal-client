import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { sortCoursebag, getCoursebagSortedIndex } from '../../helpers/coursebag';
import { RootState } from '../store';
import type { CourseGQLData } from '../../types/types';

const initialState: { coursebag?: CourseGQLData[] } = {};

export const coursebagSlice = createSlice({
  name: 'coursebag',
  initialState,
  reducers: {
    setCoursebag(state, action: PayloadAction<CourseGQLData[]>) {
      state.coursebag = sortCoursebag(action.payload);
    },
    addCourseToBagState: (state, action: PayloadAction<CourseGQLData>) => {
      if (!state.coursebag) return;
      const courseIndex = getCoursebagSortedIndex(state.coursebag, action.payload);
      if (courseIndex === -1) state.coursebag.push(action.payload);
      else state.coursebag.splice(courseIndex, 0, action.payload);
    },
    removeCourseFromBagState: (state, action: PayloadAction<CourseGQLData>) => {
      state.coursebag = state.coursebag?.filter((course) => course.id !== action.payload.id);
    },
  },
});

export const { setCoursebag, addCourseToBagState, removeCourseFromBagState } = coursebagSlice.actions;

export const selectCoursebag = (state: RootState) => state.coursebag.coursebag;

export default coursebagSlice.reducer;
