import { createSlice, PayloadAction } from '@reduxjs/toolkit';

interface ScheduleState {
  currentWeek: string;
  currentQuarter: string;
}

const initialState: ScheduleState = {
  currentWeek: '',
  currentQuarter: '',
};

export const scheduleSlice = createSlice({
  name: 'schedule',
  initialState,
  reducers: {
    setCurrentWeek: (state, action: PayloadAction<string>) => {
      state.currentWeek = action.payload;
    },
    setCurrentQuarter: (state, action: PayloadAction<string>) => {
      state.currentQuarter = action.payload;
    },
  },
});

export const { setCurrentWeek, setCurrentQuarter } = scheduleSlice.actions;

export default scheduleSlice.reducer;
