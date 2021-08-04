import { createSlice, PayloadAction } from '@reduxjs/toolkit'
import type { RootState } from '../store'
import { ReviewData } from '../../types/types';

// Define a type for the slice state
interface ReviewState {
    reviews: ReviewData[];
}

// Define the initial state using that type
const initialState: ReviewState = {
    reviews: []
}

export const reviewSlice = createSlice({
    name: 'review',
    // `createSlice` will infer the state type from the `initialState` argument
    initialState,
    reducers: {
        // Use the PayloadAction type to declare the contents of `action.payload`
        addReview: (state, action: PayloadAction<ReviewData>) => {
            state.reviews.push(action.payload);
        },
        setReviews: (state, action: PayloadAction<ReviewData[]>) => {
            state.reviews = action.payload
        },
    },
})

export const { addReview, setReviews } = reviewSlice.actions

// Other code such as selectors can use the imported `RootState` type
export const selectReviews = (state: RootState) => state.review.reviews;

export default reviewSlice.reducer