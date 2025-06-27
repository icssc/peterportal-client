import { FC, useCallback, useEffect, useState } from 'react';
import SubReview from '../Review/SubReview';
import ReviewsGrid from '../ReviewsGrid/ReviewsGrid';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { selectReviews, setReviews } from '../../store/slices/reviewSlice';
import trpc from '../../trpc';

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
    <ReviewsGrid
      title="Your Reviews"
      description="Deleting a review will remove it permanently."
      isLoading={reviewsLoading}
      noData={reviews.length === 0}
      noDataMsg="You haven't reviewed any courses yet. Look up a course you've taken to review it!"
    >
      {reviews.map((review) => (
        <div key={`user-review-${review.id}`}>
          <SubReview review={review} />
        </div>
      ))}
    </ReviewsGrid>
  );
};

export default UserReviews;
