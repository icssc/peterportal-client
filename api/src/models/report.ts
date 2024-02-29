import mongoose from 'mongoose';

const reportSchema = new mongoose.Schema({
  reviewId: {
    type: String,
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

const Report = mongoose.model('Report', reportSchema);

export default Report;
