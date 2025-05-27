import React from 'react';
import './UnreadDot.scss';

interface UnreadDotProps {
  show: boolean;
}

const UnreadDot: React.FC<UnreadDotProps> = ({ show }) => {
  if (!show) return null;

  return <div className="unread-dot">NEW</div>;
};

export default UnreadDot;
