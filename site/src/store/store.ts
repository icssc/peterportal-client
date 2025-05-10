import { configureStore } from '@reduxjs/toolkit';
import courseRequirementsReducer from './slices/courseRequirementsSlice';
import coursebagReducer from './slices/coursebagSlice';
import reviewReducer from './slices/reviewSlice';
import uiReducer from './slices/uiSlice';
import popupReducer from './slices/popupSlice';
import roadmapReducer from './slices/roadmapSlice';
import searchReducer from './slices/searchSlice';
import transferCreditsReducer from './slices/transferCreditsSlice';

export const store = configureStore({
  reducer: {
    courseRequirements: courseRequirementsReducer,
    coursebag: coursebagReducer,
    review: reviewReducer,
    ui: uiReducer,
    popup: popupReducer,
    roadmap: roadmapReducer,
    search: searchReducer,
    transferCredits: transferCreditsReducer,
  },
});

// Infer the `RootState` and `AppDispatch` types from the store itself
export type RootState = ReturnType<typeof store.getState>;
// Inferred type
export type AppDispatch = typeof store.dispatch;
