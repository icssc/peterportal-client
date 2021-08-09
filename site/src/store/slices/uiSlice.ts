import { createSlice, PayloadAction } from '@reduxjs/toolkit'
import type { RootState } from '../store'

// Define a type for the slice state
interface UIState {
    sidebarOpen: boolean;
}

// Define the initial state using that type
const initialState: UIState = {
    sidebarOpen: false
}

export const reviewSlice = createSlice({
    name: 'ui',
    // `createSlice` will infer the state type from the `initialState` argument
    initialState,
    reducers: {
        // Use the PayloadAction type to declare the contents of `action.payload`
        setSidebarStatus: (state, action: PayloadAction<boolean>) => {
            state.sidebarOpen = action.payload;
        }
    },
})

export const { setSidebarStatus } = reviewSlice.actions

// Other code such as selectors can use the imported `RootState` type
export const selectSidebarOpen = (state: RootState) => state.ui.sidebarOpen;

export default reviewSlice.reducer