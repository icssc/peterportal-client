import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import type { RootState } from '../store';
import { UserData } from '@peterportal/types';

const initialState: { user?: Omit<UserData, 'theme'> } = {};

export const userSlice = createSlice({
  name: 'user',
  initialState,
  reducers: {
    setUser: (state, action: PayloadAction<UserData>) => {
      state.user = action.payload;
    },
  },
});

export const { setUser } = userSlice.actions;

export const selectUser = (state: RootState) => state.user.user;
export const selectIsAdmin = (state: RootState) => state.user.user?.isAdmin;

export default userSlice.reducer;
