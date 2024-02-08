import mongoose from 'mongoose';

const tags = [
  'Clear grading criteria',
  'Tough grader',
  'Amazing lectures',
  'Test heavy',
  'Get ready to read',
  'Extra credit',
  'Participation matters',
  'Graded by few things',
  "Skip class? You won't pass",
  'Accessible outside class',
  'Beware of pop quizzes',
  'Lots of homework',
  'So many papers',
  'Lecture heavy',
  'Group projects',
  'Gives good feedback',
];
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
    type: mongoose.Schema.Types.ObjectId,
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
    match: /^20[0-9]{2} (Winter|Summer|Fall|Spring)$/,
  },
  timestamp: {
    type: Date,
    required: true,
    default: Date.now,
  },
  gradeReceived: {
    type: String,
    required: true,
    enum: ['A+', 'A', 'A-', 'B+', 'B', 'B-', 'C+', 'C', 'C-', 'D+', 'D', 'D-', 'F', 'P', 'NP'],
  },
  forCredit: {
    type: Boolean,
    required: false,
  },
  takeAgain: {
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
