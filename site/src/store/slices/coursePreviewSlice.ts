import { createSlice, PayloadAction } from '@reduxjs/toolkit';

interface Preview {
  type: 'course' | 'professor';
  id: string;
}

export const coursePreviewSlice = createSlice({
  name: 'coursePreview',
  initialState: {
    previewStack: [] as Preview[],
  },
  reducers: {
    addPreview: (state, action: PayloadAction<Preview>) => {
      state.previewStack.push(action.payload);
    },
    removePreview: (state) => {
      state.previewStack.pop();
    },
    clearPreviews: (state) => {
      state.previewStack = [];
    },
  },
});

export const { addPreview, removePreview, clearPreviews } = coursePreviewSlice.actions;

export default coursePreviewSlice.reducer;
