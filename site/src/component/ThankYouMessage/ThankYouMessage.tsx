import React from 'react';
import { Icon } from 'semantic-ui-react';
import './ThankYouMessage.scss';

interface ThankYouMessageProps {
  message: string;
}

const ThankYouMessage: React.FC<ThankYouMessageProps> = ({ message }) => {
  return (
    <div className="thank-you-message">
      <Icon name="check circle" size="huge" />
      <h1>Thank You</h1>
      <p>{message}</p>
    </div>
  );
};

export default ThankYouMessage;
