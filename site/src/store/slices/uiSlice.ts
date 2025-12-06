import { createSlice, PayloadAction } from '@reduxjs/toolkit';

// Define a type for the slice state
interface UIState {
  filterOpen: boolean;
}

// Define the initial state using that type
const initialState: UIState = {
  filterOpen: false,
};

export const reviewSlice = createSlice({
  name: 'ui',
  // `createSlice` will infer the state type from the `initialState` argument
  initialState,
  reducers: {
    // Use the PayloadAction type to declare the contents of `action.payload`
    setFilterStatus: (state, action: PayloadAction<boolean>) => {
      state.filterOpen = action.payload;
    },
  },
});

export const { setFilterStatus } = reviewSlice.actions;

export default reviewSlice.reducer;
