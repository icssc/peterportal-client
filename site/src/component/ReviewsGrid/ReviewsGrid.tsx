import { FC } from 'react';
import './ReviewsGrid.scss';

interface ReviewsGridProps {
  title: string;
  description: string;
  isLoading: boolean;
  noData: boolean;
  noDataMsg: string;
  children: React.ReactNode;
}

const ReviewsGrid: FC<ReviewsGridProps> = ({ title, description, isLoading, noData, noDataMsg, children }) => {
  return (
    <div className="content-wrapper reviews-container">
      <h1>{title}</h1>
      <p>{description}</p>
      {isLoading ? (
        <p>Loading...</p>
      ) : (
        <div className="reviews-grid">{noData ? <span>{noDataMsg}</span> : children}</div>
      )}
    </div>
  );
};

export default ReviewsGrid;
