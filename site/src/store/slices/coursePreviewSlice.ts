import { createSlice, PayloadAction } from '@reduxjs/toolkit';

export const coursePreviewSlice = createSlice({
  name: 'coursePreview',
  initialState: {
    courseId: '',
    professorId: '',
  },
  reducers: {
    setPreviewedCourse: (state, action: PayloadAction<string>) => {
      state.courseId = action.payload;
    },
  },
});

export const { setPreviewedCourse } = coursePreviewSlice.actions;

export default coursePreviewSlice.reducer;
