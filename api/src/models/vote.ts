import mongoose from 'mongoose';

const voteSchema = new mongoose.Schema({
  userID: {
    type: String,
    ref: 'User',
  },
  reviewID: {
    type: String,
    ref: 'Review',
  },
  timestamp: {
    type: Date,
    required: true,
    default: Date.now,
  },
});

const Vote = mongoose.model('Vote', voteSchema);

export default Vote;
