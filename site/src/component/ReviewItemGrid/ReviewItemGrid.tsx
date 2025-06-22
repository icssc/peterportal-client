import React from 'react';
import './ReviewItemGrid.scss';

const ReviewItemGrid: React.FC<{ children: React.ReactNode }> = ({ children }) => {
  return <div className="reviews-grid">{children}</div>;
};

export default ReviewItemGrid;
