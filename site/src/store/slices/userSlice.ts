import { createSlice } from '@reduxjs/toolkit';
import type { RootState } from '../store';
import { UserSliceState } from '@peterportal/types';

const initialState: UserSliceState = {
  user: null,
  theme: 'system',
};

export const userSlice = createSlice({
  name: 'user',
  initialState,
  reducers: {},
});

// Other code such as selectors can use the imported `RootState` type
export const selectSidebarOpen = (state: RootState) => state.ui.sidebarOpen;

export default userSlice.reducer;
