import { createSlice, PayloadAction } from '@reduxjs/toolkit';

export const uiSlice = createSlice({
  name: 'ui',
  initialState: {
    changelogOpen: false,
  },
  reducers: {
    setChangelogOpen: (state, action: PayloadAction<boolean>) => {
      state.changelogOpen = action.payload;
    },
  },
});

export const { setChangelogOpen } = uiSlice.actions;

export default uiSlice.reducer;
