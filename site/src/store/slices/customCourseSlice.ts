import { createSlice, PayloadAction } from '@reduxjs/toolkit';

interface CustomCourse {
  id: number;
  courseName: string;
  units: number;
  description: string;
}

export const customCoursesSlice = createSlice({
  name: 'customCourses',
  initialState: {
    userCustomCourses: [] as CustomCourse[],
  },
  reducers: {
    addCustomCourse: (state, action: PayloadAction<CustomCourse>) => {
      state.userCustomCourses.push(action.payload);
    },
    removeCustomCourse: (state, action: PayloadAction<number>) => {
      state.userCustomCourses = state.userCustomCourses.filter((course) => course.id !== action.payload);
    },
    updateCustomCourse: (state, action: PayloadAction<CustomCourse>) => {
      const course = state.userCustomCourses.find((course) => course.id === action.payload.id);
      if (course) {
        course.courseName = action.payload.courseName;
        course.units = action.payload.units;
        course.description = action.payload.description;
      }
    },
    reorderCustomCourses: (state, action) => {
      const { oldIndex, newIndex } = action.payload;
      const movedCourse = state.userCustomCourses.splice(oldIndex, 1)[0];
      state.userCustomCourses.splice(newIndex, 0, movedCourse);
    },
  },
});

export const { addCustomCourse, removeCustomCourse, updateCustomCourse, reorderCustomCourses } =
  customCoursesSlice.actions;

export default customCoursesSlice.reducer;
