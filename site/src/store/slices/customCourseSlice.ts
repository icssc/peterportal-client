import { createSlice, PayloadAction } from '@reduxjs/toolkit';

interface CustomCourse {
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
    removeCustomCourse: (state, action: PayloadAction<string>) => {
      state.userCustomCourses = state.userCustomCourses.filter((course) => course.courseName !== action.payload);
    },
    updateCustomCourse: (state, action: PayloadAction<CustomCourse>) => {
      const course = state.userCustomCourses.find((course) => course.courseName === action.payload.courseName);
      if (course) {
        course.units = action.payload.units;
        course.description = action.payload.description;
      }
    },
  },
});

export const { addCustomCourse, removeCustomCourse } = customCoursesSlice.actions;

export default customCoursesSlice.reducer;
