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
    setAllCatalogYears: (state, action: PayloadAction<Record<string, string>>) => {
      state.catalogYear = action.payload;
    },
  },
});

export const { setCatalogYear, setAllCatalogYears } = catalogYearSlice.actions;

export default catalogYearSlice.reducer;
