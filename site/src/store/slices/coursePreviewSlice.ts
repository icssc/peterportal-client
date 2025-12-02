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
    setPreviewedProfessor: (state, action: PayloadAction<string>) => {
      state.professorId = action.payload;
    },
  },
});

export const { setPreviewedCourse, setPreviewedProfessor } = coursePreviewSlice.actions;

export default coursePreviewSlice.reducer;
