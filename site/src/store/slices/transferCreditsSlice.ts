import { createSlice, PayloadAction } from '@reduxjs/toolkit';

interface UncategorizedCourseEntry {
  name: string | null;
  units: number | null;
}

export const transferCreditsSlice = createSlice({
  name: 'transferCredits',
  initialState: {
    showTransfersMenu: false,
    uncategorizedCourses: [] as UncategorizedCourseEntry[],
  },
  reducers: {
    setShowTransfersMenu: (state, action: PayloadAction<boolean>) => {
      state.showTransfersMenu = action.payload;
    },
    setUncategorizedCourses: (state, action: PayloadAction<UncategorizedCourseEntry[]>) => {
      state.uncategorizedCourses = action.payload;
    },
    removeUncategorizedCourse: (state, action: PayloadAction<UncategorizedCourseEntry>) => {
      state.uncategorizedCourses = state.uncategorizedCourses.filter(
        (course) => course.name !== action.payload.name || course.units !== action.payload.units,
      );
    },
  },
});

export const { setShowTransfersMenu, setUncategorizedCourses, removeUncategorizedCourse } =
  transferCreditsSlice.actions;

export default transferCreditsSlice.reducer;
