import { FC, Children } from 'react';
import './ReviewGridTemplate.scss';

interface ReviewGridTemplateProps {
  title: string;
  description: string;
  isLoading: boolean;
  noDataMsg: string;
  children: React.ReactNode;
}

const ReviewGridTemplate: FC<ReviewGridTemplateProps> = ({ title, description, isLoading, noDataMsg, children }) => {
  return (
    <div className="content-wrapper review-grid-template">
      <h1>{title}</h1>
      <p>{description}</p>
      {isLoading ? (
        <p>Loading...</p>
      ) : (
        <div className="review-grid">
          {Children.toArray(children).length === 0 ? <span>{noDataMsg}</span> : children}
        </div>
      )}
    </div>
  );
};

export default ReviewGridTemplate;
