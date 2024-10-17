import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import type { RootState } from '../store';
import { EditReviewSubmission, ReviewData } from '@peterportal/types';

// Define a type for the slice state
interface ReviewState {
  reviews: ReviewData[];
  formOpen: boolean;
}

// Define the initial state using that type
const initialState: ReviewState = {
  reviews: [],
  formOpen: false,
};

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
      state.reviews = action.payload;
    },
    editReview: (state, action: PayloadAction<EditReviewSubmission>) => {
      const i = state.reviews.findIndex((review) => review._id === action.payload._id);
      state.reviews[i] = { ...state.reviews[i], ...action.payload }; // overwrite with edited properties
    },
    setFormStatus: (state, action: PayloadAction<boolean>) => {
      state.formOpen = action.payload;
    },
    toggleFormStatus: (state) => {
      state.formOpen = !state.formOpen;
    },
  },
});

export const { addReview, setReviews, editReview, setFormStatus, toggleFormStatus } = reviewSlice.actions;

// Other code such as selectors can use the imported `RootState` type
export const selectReviews = (state: RootState) => state.review.reviews;

export default reviewSlice.reducer;
