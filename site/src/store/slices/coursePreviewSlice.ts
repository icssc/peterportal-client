import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { ToastSeverity } from '../../helpers/toast';

export const coursePreviewSlice = createSlice({
  name: 'coursePreview',
  initialState: {
    courseId: '',
    professorId: '',
    toastMsg: '',
    toastSeverity: 'error' as ToastSeverity,
    showToast: false,
  },
  reducers: {
    setPreviewedCourse: (state, action: PayloadAction<string>) => {
      state.courseId = action.payload;
      state.professorId = '';
    },
    setPreviewedProfessor: (state, action: PayloadAction<string>) => {
      state.professorId = action.payload;
      state.courseId = '';
    },
    setToastMsg: (state, action: PayloadAction<string>) => {
      state.toastMsg = action.payload;
    },
    setToastSeverity: (state, action: PayloadAction<ToastSeverity>) => {
      state.toastSeverity = action.payload;
    },
    setShowToast: (state, action: PayloadAction<boolean>) => {
      state.showToast = action.payload;
    },
  },
});

export const { setPreviewedCourse, setPreviewedProfessor, setToastMsg, setToastSeverity, setShowToast } =
  coursePreviewSlice.actions;

export default coursePreviewSlice.reducer;
