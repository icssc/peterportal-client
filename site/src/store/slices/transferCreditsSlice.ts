import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import {
  APExam,
  TransferredGE,
  TransferredCourse,
  TransferredAPExam,
  TransferredUncategorized,
} from '@peterportal/types';

type DataLoadingState = 'waiting' | 'loading' | 'done';

export const transferCreditsSlice = createSlice({
  name: 'transferCredits',
  initialState: {
    showTransfersMenu: false,
    dataLoadState: 'waiting' as DataLoadingState,
    transferredCourses: [] as TransferredCourse[],
    apExamInfo: [] as APExam[],
    userAPExams: [] as TransferredAPExam[],
    transferredGEs: [] as TransferredGE[],
    uncategorizedCourses: [] as TransferredUncategorized[],
  },
  reducers: {
    setShowTransfersMenu: (state, action: PayloadAction<boolean>) => {
      state.showTransfersMenu = action.payload;
    },
    setDataLoadState: (state, action: PayloadAction<DataLoadingState>) => {
      state.dataLoadState = action.payload;
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
    setAPExams: (state, action: PayloadAction<APExam[]>) => {
      state.apExamInfo = action.payload;
    },
    setUserAPExams: (state, action: PayloadAction<TransferredAPExam[]>) => {
      state.userAPExams = action.payload;
    },
    addUserAPExam: (state, action: PayloadAction<TransferredAPExam>) => {
      state.userAPExams.push(action.payload);
    },
    removeUserAPExam: (state, action: PayloadAction<string>) => {
      state.userAPExams = state.userAPExams.filter((exam) => exam.examName !== action.payload);
    },
    updateUserExam: (state, action: PayloadAction<TransferredAPExam>) => {
      const e = state.userAPExams.find((exam) => exam.examName === action.payload.examName);
      if (e) {
        e.score = action.payload.score;
        e.units = action.payload.units;
      }
    },
    setAllTransferredGEs: (state, action: PayloadAction<TransferredGE[]>) => {
      state.transferredGEs = action.payload;
    },
    setTransferredGE: (state, action: PayloadAction<TransferredGE>) => {
      const foundGE = state.transferredGEs.find((ge) => ge.geName === action.payload.geName);
      if (foundGE) Object.assign(foundGE, action.payload);
      else state.transferredGEs.push(action.payload);
    },
    setUncategorizedCourses: (state, action: PayloadAction<TransferredUncategorized[]>) => {
      state.uncategorizedCourses = action.payload;
    },
    removeUncategorizedCourse: (state, action: PayloadAction<TransferredUncategorized>) => {
      state.uncategorizedCourses = state.uncategorizedCourses.filter(
        (course) => course.name !== action.payload.name || course.units !== action.payload.units,
      );
    },
  },
});

export const {
  setShowTransfersMenu,
  setDataLoadState,
  addTransferredCourse,
  removeTransferredCourse,
  updateTransferredCourse,
  setTransferredCourses,
  setAPExams,
  setUserAPExams,
  addUserAPExam,
  removeUserAPExam,
  updateUserExam,
  setAllTransferredGEs,
  setTransferredGE,
  setUncategorizedCourses,
  removeUncategorizedCourse,
} = transferCreditsSlice.actions;

export default transferCreditsSlice.reducer;
