import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { APExam, TransferredGE } from '@peterportal/types';

export interface TransferredCourse {
  courseName: string;
  units: number;
}

export interface UserAPExam {
  examName: string;
  score: number;
  units: number;
}

interface UncategorizedCourseEntry {
  name: string | null;
  units: number | null;
}

export const transferCreditsSlice = createSlice({
  name: 'transferCredits',
  initialState: {
    showTransfersMenu: false,
    userDataLoaded: false,
    transferredCourses: [] as TransferredCourse[],
    apExamInfo: [] as APExam[],
    userAPExams: [] as UserAPExam[],
    transferredGEs: [] as TransferredGE[],
    uncategorizedCourses: [] as UncategorizedCourseEntry[],
  },
  reducers: {
    setShowTransfersMenu: (state, action: PayloadAction<boolean>) => {
      state.showTransfersMenu = action.payload;
    },
    setUserDataLoaded: (state, action: PayloadAction<boolean>) => {
      state.userDataLoaded = action.payload;
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
    setUserAPExams: (state, action: PayloadAction<UserAPExam[]>) => {
      state.userAPExams = action.payload;
    },
    addUserAPExam: (state, action: PayloadAction<UserAPExam>) => {
      state.userAPExams.push(action.payload);
    },
    removeUserAPExam: (state, action: PayloadAction<string>) => {
      state.userAPExams = state.userAPExams.filter((exam) => exam.examName !== action.payload);
    },
    updateUserExam: (state, action: PayloadAction<UserAPExam>) => {
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

export const {
  setShowTransfersMenu,
  setUserDataLoaded,
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
