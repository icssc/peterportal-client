import mongoose from 'mongoose';

const preferenceSchema = new mongoose.Schema({
  theme: {
    type: String,
    required: true,
  },
});

const Preference = mongoose.model('Preference', preferenceSchema);

export default Preference;
