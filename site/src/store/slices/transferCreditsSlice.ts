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
      const transferredGEs: TransferredGE[] = state.transferredGEs.filter((ge) => ge.geName !== action.payload.geName);
      transferredGEs.push(action.payload);
      state.transferredGEs = transferredGEs;
    },
  },
});

export const { setShowTransfersMenu, setAllTransferredGEs, setTransferredGE } = transferCreditsSlice.actions;

export default transferCreditsSlice.reducer;
