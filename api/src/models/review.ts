import { grades, tags } from '@peterportal/types';
import mongoose from 'mongoose';

const reviewSchema = new mongoose.Schema({
  professorID: {
    type: String,
    required: true,
  },
  courseID: {
    type: String,
    required: true,
  },
  userID: {
    type: String,
    ref: 'User',
    required: true,
  },
  userDisplay: {
    type: String,
    required: true,
    default: 'Anonymous Peter',
  },
  reviewContent: {
    type: String,
    required: false,
    default: '',
  },
  rating: {
    type: Number,
    required: true,
    default: 3,
    min: 0,
    max: 5,
  },
  difficulty: {
    type: Number,
    required: true,
    min: 0,
    default: 3,
    max: 5,
  },
  score: {
    type: Number,
    required: false,
    min: 0,
    default: 0,
  },
  quarter: {
    type: String,
    required: true,
  },
  timestamp: {
    type: Date,
    required: true,
    default: Date.now,
  },
  gradeReceived: {
    type: String,
    required: true,
    enum: grades,
  },
  forCredit: {
    type: Boolean,
    required: false,
  },
  takeAgain: {
    type: Boolean,
    required: false,
  },
  textbook: {
    type: Boolean,
    required: false,
  },
  attendance: {
    type: Boolean,
    required: false,
  },
  tags: {
    type: [String],
    required: false,
    enum: tags,
  },
  verified: {
    type: Boolean,
    required: true,
    default: false,
  },
});

const Review = mongoose.model('Review', reviewSchema);

export default Review;
