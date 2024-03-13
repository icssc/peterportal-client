import mongoose from 'mongoose';

const voteSchema = new mongoose.Schema({
  userID: {
    type: String,
    required: true,
  },
  reviewID: {
    type: mongoose.Schema.Types.ObjectId,
    ref: 'Review',
    required: true,
  },
  timestamp: {
    type: Date,
    required: true,
    default: Date.now,
  },
  score: {
    type: Number,
    required: true,
    enum: [-1, 1],
  },
});

const Vote = mongoose.model('Vote', voteSchema);

export default Vote;
