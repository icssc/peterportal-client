import { FC, useCallback, useEffect, useState } from 'react';
import SubReview from '../../component/Review/SubReview';
import './UserReviews.scss';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { selectReviews, setReviews } from '../../store/slices/reviewSlice';
import { ReviewData } from '@peterportal/types';
import trpc from '../../trpc';

const UserReviewsList: FC<{ reviews: ReviewData[] }> = ({ reviews }) => {
  return (
    <div className="user-reviews-container">
      {reviews.length == 0 && (
        <span>You haven't reviewed any courses yet. Look up a course you've taken to review it!</span>
      )}
      {reviews.map((review) => (
        <div key={review.id!} className="user-review-wrapper">
          <SubReview review={review} />
        </div>
      ))}
    </div>
  );
};

const UserReviews: FC = () => {
  const reviews = useAppSelector(selectReviews);
  const [reviewsLoading, setReviewsLoading] = useState(true);
  const dispatch = useAppDispatch();

  const getUserReviews = useCallback(async () => {
    const response = await trpc.reviews.getUsersReviews.query();
    dispatch(setReviews(response));
    setReviewsLoading(false);
  }, [dispatch]);

  useEffect(() => {
    getUserReviews();
  }, [getUserReviews]);

  /** @todo replace the loading text here with LoadingSpinner once that gets merged */
  return (
    <div className="content-wrapper">
      <h1>Your Reviews</h1>
      <p>Deleting a review will remove it permanently.</p>
      {reviewsLoading ? <p>Loading...</p> : <UserReviewsList reviews={reviews} />}
    </div>
  );
};

export default UserReviews;
