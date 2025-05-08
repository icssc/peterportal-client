import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { TransferredGE } from '@peterportal/types';

export const transferCreditsSlice = createSlice({
  name: 'transferCredits',
  initialState: {
    showTransfersMenu: false,
    transferredGEs: [] as TransferredGE[],
  },
  reducers: {
    setShowTransfersMenu: (state, action: PayloadAction<boolean>) => {
      state.showTransfersMenu = action.payload;
    },
    setAllTransferredGEs: (state, action: PayloadAction<TransferredGE[]>) => {
      state.transferredGEs = action.payload;
    },
    setTransferredGE: (state, action: PayloadAction<TransferredGE>) => {
      state.transferredGEs = state.transferredGEs.map((ge) => {
        return ge.geName === action.payload.geName ? action.payload : ge;
      });
    },
  },
});

export const { setShowTransfersMenu, setAllTransferredGEs, setTransferredGE } = transferCreditsSlice.actions;

export default transferCreditsSlice.reducer;
