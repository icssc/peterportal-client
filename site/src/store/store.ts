import { configureStore } from '@reduxjs/toolkit'
import reviewReducer from './slices/reviewSlice';
import uiReducer from './slices/uiSlice';
import popupReducer from './slices/popupSlice';
import roadmapReducer from './slices/roadmapSlice';

export const store = configureStore({
    reducer: {
        review: reviewReducer,
        ui: uiReducer,
        popup: popupReducer,
        roadmap: roadmapReducer
    }
})

// Infer the `RootState` and `AppDispatch` types from the store itself
export type RootState = ReturnType<typeof store.getState>
// Inferred type
export type AppDispatch = typeof store.dispatch