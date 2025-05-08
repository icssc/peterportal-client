import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { APExam } from '@peterportal/types';

interface userAPExam {
  examName: string;
  score: number;
  units: number;
}

export const transferCreditsSlice = createSlice({
  name: 'transferCredits',
  initialState: {
    showTransfersMenu: false,
    apExamInfo: [] as APExam[],
    userAPExams: [] as userAPExam[],
  },
  reducers: {
    setShowTransfersMenu: (state, action: PayloadAction<boolean>) => {
      state.showTransfersMenu = action.payload;
    },
    setAPExams: (state, action: PayloadAction<APExam[]>) => {
      state.apExamInfo = action.payload;
    },
    addUserAPExam: (state, action: PayloadAction<userAPExam>) => {
      if (!state.userAPExams.find((exam) => exam.examName === action.payload.examName)) {
        state.userAPExams.push(action.payload);
      }
    },
    removeUserAPExam: (state, action: PayloadAction<string>) => {
      state.userAPExams = state.userAPExams.filter((exam) => exam.examName !== action.payload);
    },
    updateUserExam: (state, action: PayloadAction<userAPExam>) => {
      const e = state.userAPExams.find((exam) => exam.examName === action.payload.examName);
      if (e) {
        e.score = action.payload.score;
        e.units = action.payload.units;
      }
    },
  },
});

export const { setShowTransfersMenu, setAPExams, addUserAPExam, removeUserAPExam, updateUserExam } =
  transferCreditsSlice.actions;

export default transferCreditsSlice.reducer;
