import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { APExams } from '@peterportal/types';

type APExam = APExams[number];

interface userAPExam {
  examName: string;
  score: number;
  units: number;
}

export const transferCreditsSlice = createSlice({
  name: 'transferCredits',
  initialState: {
    showTransfersMenu: false,
    APExams: [] as APExam[],
    userAPExams: [] as userAPExam[],
  },
  reducers: {
    setShowTransfersMenu: (state, action: PayloadAction<boolean>) => {
      state.showTransfersMenu = action.payload;
    },
    setAPExams: (state, action: PayloadAction<APExam[]>) => {
      state.APExams = action.payload;
    },
    addUserAPExam: (state, action: PayloadAction<userAPExam>) => {
      if (!state.userAPExams.find((exam) => exam.examName === action.payload.examName)) {
        state.userAPExams.push(action.payload);
      }
    },
    removeUserAPExam: (state, action: PayloadAction<string>) => {
      state.userAPExams = state.userAPExams.filter((exam) => exam.examName !== action.payload);
    },
  },
});

export const { setShowTransfersMenu, setAPExams, addUserAPExam, removeUserAPExam } = transferCreditsSlice.actions;

export default transferCreditsSlice.reducer;
