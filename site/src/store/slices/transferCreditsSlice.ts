import { createSlice, PayloadAction } from '@reduxjs/toolkit';

export const transferCreditsSlice = createSlice({
  name: 'transferCredits',
  initialState: {
    showTransfersMenu: false,
  },
  reducers: {
    setShowTransfersMenu: (state, action: PayloadAction<boolean>) => {
      state.showTransfersMenu = action.payload;
    },
  },
});

export const { setShowTransfersMenu } = transferCreditsSlice.actions;

export default transferCreditsSlice.reducer;
