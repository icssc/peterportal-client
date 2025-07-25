import { FC, useCallback, useEffect, useState } from 'react';
import SubReview from '../../component/Review/SubReview';
import './UserReviews.scss';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { selectReviews, setReviews } from '../../store/slices/reviewSlice';
import { ReviewData } from '@peterportal/types';
import ReviewItemGrid from '../../component/ReviewItemGrid/ReviewItemGrid';
import trpc from '../../trpc';
import LoadingSpinner from '../../component/LoadingSpinner/LoadingSpinner';

const UserReviewsList: FC<{ reviews: ReviewData[] }> = ({ reviews }) => {
  return (
    <ReviewItemGrid>
      {reviews.length == 0 && (
        <span>You haven't reviewed any courses yet. Look up a course you've taken to review it!</span>
      )}
      {reviews.map((review) => (
        <SubReview key={'user-review-' + review.id!} review={review} />
      ))}
    </ReviewItemGrid>
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

  return (
    <div className="content-wrapper user-reviews-page">
      <h1>Your Reviews</h1>
      <p>Deleting a review will remove it permanently.</p>
      {reviewsLoading ? <LoadingSpinner /> : <UserReviewsList reviews={reviews} />}
    </div>
  );
};

export default UserReviews;
