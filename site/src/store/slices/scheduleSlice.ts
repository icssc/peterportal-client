import { createSlice, createAsyncThunk } from '@reduxjs/toolkit';
import trpc from '../../trpc';

interface ScheduleState {
  currentWeek: string | null;
}

const initialState: ScheduleState = {
  currentWeek: null,
};

export const fetchCurrentWeek = createAsyncThunk('schedule/fetchCurrentWeek', async () => {
  const res = await trpc.schedule.currentWeek.query();
  return res.display.split(' • ')[0];
});

export const scheduleSlice = createSlice({
  name: 'schedule',
  initialState,
  reducers: {},
  extraReducers: (builder) => {
    builder.addCase(fetchCurrentWeek.fulfilled, (state, action) => {
      state.currentWeek = action.payload;
    });
  },
});

export default scheduleSlice.reducer;
