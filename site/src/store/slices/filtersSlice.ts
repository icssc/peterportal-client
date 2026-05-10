import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { QuarterFilterName } from '../../helpers/quarterFilter';

export const filtersSlice = createSlice({
  name: 'filters',
  initialState: {
    quarterFilters: [] as QuarterFilterName[],
  },
  reducers: {
    toggleQuarterFilter: (state, action: PayloadAction<QuarterFilterName>) => {
      const quarter = action.payload;
      const idx = state.quarterFilters.indexOf(quarter);
      if (idx === -1) {
        state.quarterFilters.push(quarter);
      } else {
        state.quarterFilters.splice(idx, 1);
      }
    },
  },
});

export const { toggleQuarterFilter } = filtersSlice.actions;

export default filtersSlice.reducer;
