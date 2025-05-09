import React from 'react';
import './ThankYouMessage.scss';
import { CheckCircleFill } from 'react-bootstrap-icons';

interface ThankYouMessageProps {
  message: string;
}

const ThankYouMessage: React.FC<ThankYouMessageProps> = ({ message }) => {
  return (
    <div className="thank-you-message">
      <CheckCircleFill width={64} height={64} />
      <h1>Thank You</h1>
      <p>{message}</p>
    </div>
  );
};

export default ThankYouMessage;
