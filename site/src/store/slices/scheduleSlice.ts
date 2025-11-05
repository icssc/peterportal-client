import { createSlice, createAsyncThunk, PayloadAction } from '@reduxjs/toolkit';
import trpc from '../../trpc';

interface ScheduleState {
  currentWeek: string;
  currentQuarter: string;
}

const initialState: ScheduleState = {
  currentWeek: '',
  currentQuarter: '',
};

// export const fetchCurrentWeek = createAsyncThunk('schedule/fetchCurrentWeek', async () => {
//   const res = await trpc.schedule.currentWeek.query();
//   return res.display.split(' • ')[0];
// });

export const fetchCurrentQuarter = createAsyncThunk('schedule/fetchCurrentQuarter', async () => {
  const res = await trpc.schedule.currentQuarter.query();
  return res;
});

export const scheduleSlice = createSlice({
  name: 'schedule',
  initialState,
  reducers: {
    setCurrentWeek: (state, action: PayloadAction<string>) => {
      state.currentWeek = action.payload;
    },
  },
  extraReducers: (builder) => {
    // builder.addCase(fetchCurrentWeek.fulfilled, (state, action) => {
    //   state.currentWeek = action.payload;
    // });
    builder.addCase(fetchCurrentQuarter.fulfilled, (state, action) => {
      state.currentQuarter = action.payload;
    });
  },
});

export const { setCurrentWeek } = scheduleSlice.actions;

export default scheduleSlice.reducer;
