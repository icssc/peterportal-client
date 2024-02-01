import axios from 'axios';
import { ReviewData } from '../types/types';

export async function verifyCaptcha(review: ReviewData) {
  const reqBody = {
    secret: process.env.GRECAPTCHA_SECRET ?? '',
    response: review.captchaToken ?? '',
  };
  const query = new URLSearchParams(reqBody);
  const response = await axios
    .post('https://www.google.com/recaptcha/api/siteverify?' + query)
    .then((x) => x.data)
    .catch((e) => {
      console.error('Error validating captcha response', e);
      return { success: false };
    });

  return response;
}
