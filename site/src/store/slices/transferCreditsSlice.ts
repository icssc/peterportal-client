import { createSlice, PayloadAction } from '@reduxjs/toolkit';

export interface TransferredCourse {
  courseName: string;
  units: number;
}

export const transferCreditsSlice = createSlice({
  name: 'transferCredits',
  initialState: {
    showTransfersMenu: false,
    transferredCourses: [] as TransferredCourse[],
  },
  reducers: {
    setShowTransfersMenu: (state, action: PayloadAction<boolean>) => {
      state.showTransfersMenu = action.payload;
    },
    addTransferredCourse: (state, action: PayloadAction<TransferredCourse>) => {
      state.transferredCourses.push(action.payload);
    },
    removeTransferredCourse: (state, action: PayloadAction<string>) => {
      state.transferredCourses = state.transferredCourses.filter((course) => course.courseName !== action.payload);
    },
    updateTransferredCourse: (state, action: PayloadAction<TransferredCourse>) => {
      const course = state.transferredCourses.find((course) => course.courseName === action.payload.courseName);
      if (course) {
        course.units = action.payload.units;
      }
    },
    setTransferredCourses: (state, action: PayloadAction<TransferredCourse[]>) => {
      state.transferredCourses = action.payload;
    },
  },
});

export const {
  setShowTransfersMenu,
  addTransferredCourse,
  removeTransferredCourse,
  updateTransferredCourse,
  setTransferredCourses,
} = transferCreditsSlice.actions;

export default transferCreditsSlice.reducer;
