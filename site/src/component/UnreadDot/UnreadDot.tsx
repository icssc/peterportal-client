import React from 'react';
import './UnreadDot.scss';

interface UnreadDotProps {
  show: boolean;
  displayFullNewText: boolean;
}

const UnreadDot: React.FC<UnreadDotProps> = ({ show, displayFullNewText }) => {
  if (!show) return null;

  return <div className={`unread-${displayFullNewText ? 'pill' : 'circle'}`}>{displayFullNewText ? 'NEW' : null}</div>;
};

export default UnreadDot;
