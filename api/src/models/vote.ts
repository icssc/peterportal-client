import mongoose from 'mongoose';

const voteSchema = new mongoose.Schema({
  reviewId: {
    type: mongoose.Schema.Types.ObjectId,
    ref: 'Review',
  },
  reason: {
    type: String,
    required: true,
  },
  timestamp: {
    type: Date,
    required: true,
    default: Date.now,
  },
});

const Vote = mongoose.model('Vote', voteSchema);

export default Vote;
