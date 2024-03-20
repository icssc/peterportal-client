import mongoose from 'mongoose';

const roadmapSchema = new mongoose.Schema({
  roadmap: {
    type: mongoose.Schema.Types.Mixed,
    required: true,
  },
  userID: {
    type: String,
    required: true,
  },
});

const Roadmap = mongoose.model('Roadmap', roadmapSchema);

export default Roadmap;
