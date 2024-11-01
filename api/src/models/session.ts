import mongoose from 'mongoose';

const sessionSchema = new mongoose.Schema({
  _id: {
    type: String,
  },
  expires: {
    type: Date,
    required: true,
  },
  session: {
    cookie: {
      originalMaxAge: {
        type: Number,
      },
      expires: {
        type: Date,
      },
      secure: {
        type: Boolean,
        default: null,
      },
      httpOnly: {
        type: Boolean,
        default: true,
      },
      domain: {
        type: String,
        default: null,
      },
      path: {
        type: String,
        default: '/',
      },
      sameSite: {
        type: Boolean,
        default: null,
      },
    },
    passport: {
      user: {
        id: {
          type: String,
        },
        email: {
          type: String,
          required: true,
          match: /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$/,
        },
        name: {
          type: String,
          required: true,
        },
        picture: {
          type: String,
          required: false,
        },
      },
    },
  },
});

const Session = mongoose.model('Session', sessionSchema);

export default Session;
