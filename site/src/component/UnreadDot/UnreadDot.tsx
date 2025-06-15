import { FC } from 'react';
import './UnreadDot.scss';

interface UnreadDotProps {
  show: boolean;
  displayFullNewText: boolean;
}

const UnreadDot: FC<UnreadDotProps> = ({ show, displayFullNewText }) => {
  if (!show) return null;

  if (displayFullNewText) {
    return <div className="unread-pill">NEW</div>;
  }

  return (
    <>
      <div className="spacing" />
      <div className="unread-circle" />
    </>
  );
};

export default UnreadDot;
