import mongoose from 'mongoose';

const reportSchema = new mongoose.Schema({
  reviewID: {
    type: mongoose.Schema.Types.ObjectId,
    ref: 'Review',
    required: true,
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
