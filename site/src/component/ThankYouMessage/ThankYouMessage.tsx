import React from 'react';
import './ThankYouMessage.scss';

import CheckCircleIcon from '@mui/icons-material/CheckCircle';

interface ThankYouMessageProps {
  message: string;
}

const ThankYouMessage: React.FC<ThankYouMessageProps> = ({ message }) => {
  return (
    <div className="thank-you-message">
      <CheckCircleIcon />
      <h1>Thank You</h1>
      <p>{message}</p>
    </div>
  );
};

export default ThankYouMessage;
