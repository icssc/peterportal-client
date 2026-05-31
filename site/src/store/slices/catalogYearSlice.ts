import { createSlice, PayloadAction } from '@reduxjs/toolkit';

export const catalogYearSlice = createSlice({
  name: 'catalogYear',
  initialState: {
    catalogYear: {} as Record<string, string>,
  },
  reducers: {
    setCatalogYear: (state, action: PayloadAction<{ majorId: string; year: string }>) => {
      state.catalogYear[action.payload.majorId] = action.payload.year;
    },
  },
});

export const { setCatalogYear } = catalogYearSlice.actions;

export default catalogYearSlice.reducer;
