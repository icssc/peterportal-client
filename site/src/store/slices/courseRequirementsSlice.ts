import { createSlice, PayloadAction } from '@reduxjs/toolkit';

export type RequirementsTabName = 'Major' | 'Minor' | 'GE' | 'All Courses';

const courseRequirementsSlice = createSlice({
  name: 'courseRequirements',
  initialState: {
    selectedTab: 'Major' as RequirementsTabName,
  },
  reducers: {
    setSelectedTab: (state, action: PayloadAction<RequirementsTabName>) => {
      state.selectedTab = action.payload;
    },
  },
});

export const { setSelectedTab } = courseRequirementsSlice.actions;

export default courseRequirementsSlice.reducer;
