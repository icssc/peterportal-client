import { configureStore } from '@reduxjs/toolkit'
import reviewReducer from './slices/reviewSlice';
import uiReducer from './slices/uiSlice';

export const store = configureStore({
    reducer: {
        review: reviewReducer,
        ui: uiReducer
    }
})

// Infer the `RootState` and `AppDispatch` types from the store itself
export type RootState = ReturnType<typeof store.getState>
// Inferred type
export type AppDispatch = typeof store.dispatch