import { ReviewSubmission } from '@peterportal/types';

export async function verifyCaptcha(review: ReviewSubmission) {
  const reqBody = {
    secret: process.env.GRECAPTCHA_SECRET ?? '',
    response: review.captchaToken ?? '',
  };
  const query = new URLSearchParams(reqBody);
  const response = await fetch('https://www.google.com/recaptcha/api/siteverify?' + query, { method: 'POST' })
    .then((res) => res.json())
    .catch((e) => {
      console.error('Error validating captcha response', e);
      return { success: false };
    });

  return response;
}
