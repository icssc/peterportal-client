import { createSlice, PayloadAction } from '@reduxjs/toolkit';

interface CustomCourse {
  cardId: number;
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
      state.userCustomCourses = state.userCustomCourses.filter((course) => course.cardId !== action.payload);
    },
    updateCustomCourse: (state, action: PayloadAction<CustomCourse>) => {
      const course = state.userCustomCourses.find((course) => course.cardId === action.payload.cardId);
      if (course) {
        course.courseName = action.payload.courseName;
        course.units = action.payload.units;
        course.description = action.payload.description;
      }
    },
  },
});

export const { addCustomCourse, removeCustomCourse, updateCustomCourse } = customCoursesSlice.actions;

export default customCoursesSlice.reducer;
