import mongoose from 'mongoose';

const voteSchema = new mongoose.Schema({
  userID: {
    type: mongoose.Schema.Types.ObjectId,
    ref: 'User',
  },
  reviewID: {
    type: mongoose.Schema.Types.ObjectId,
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
