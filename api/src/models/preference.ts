import { theme } from '@peterportal/types';
import mongoose from 'mongoose';

const preferenceSchema = new mongoose.Schema({
  theme: {
    type: String,
    enum: theme.options,
    required: true,
  },
  userID: {
    type: String,
    required: true,
  },
});

const Preference = mongoose.model('Preference', preferenceSchema);

export default Preference;
