import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { RootState } from '../store';
import type { Coursebag, CourseGQLData } from '../../types/types';

const initialState: { coursebag?: Coursebag } = {};

export const coursebagSlice = createSlice({
  name: 'coursebag',
  initialState,
  reducers: {
    setCoursebag(state, action: PayloadAction<Coursebag>) {
      state.coursebag = action.payload;
    },
    addCourseToBagState: (state, action: PayloadAction<CourseGQLData>) => {
      if (!state.coursebag) return;
      const courseIndex = state.coursebag.findIndex((c) => c.id > action.payload.id);
      const spliceIndex = courseIndex === -1 ? state.coursebag.length : courseIndex;
      state.coursebag.splice(spliceIndex, 0, action.payload);
    },
    removeCourseFromBagState: (state, action: PayloadAction<CourseGQLData>) => {
      state.coursebag = state.coursebag?.filter((course) => course.id !== action.payload.id);
    },
  },
});

export const { setCoursebag, addCourseToBagState, removeCourseFromBagState } = coursebagSlice.actions;

export const selectCoursebag = (state: RootState) => state.coursebag.coursebag;

export default coursebagSlice.reducer;
