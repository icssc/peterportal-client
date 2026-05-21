import { createSlice, PayloadAction } from '@reduxjs/toolkit';

export const catalogYearSlice = createSlice({
  name: 'catalogYear',
  initialState: {
    catalogYear: '',
  },
  reducers: {
    setCatalogYear: (state, action: PayloadAction<string>) => {
      state.catalogYear = action.payload;
    },
  },
});

export const { setCatalogYear } = catalogYearSlice.actions;

export default catalogYearSlice.reducer;
