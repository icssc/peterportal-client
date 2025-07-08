import { FC, useCallback, useEffect, useState } from 'react';
import './Verify.scss';
import trpc from '../../../trpc';
import ReviewItemGrid from '../../../component/ReviewItemGrid/ReviewItemGrid';
import UnverifiedReview from './UnverifiedReview';
import { selectReviews, setReviews } from '../../../store/slices/reviewSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { ReviewData } from '@peterportal/types';
import LoadingSpinner from '../../../component/LoadingSpinner/LoadingSpinner';

const UnverifiedReviewList: FC<{ reviews: ReviewData[] }> = ({ reviews }) => {
  const dispatch = useAppDispatch();

  const verifyReview = async (reviewId: number) => {
    await trpc.reviews.verify.mutate({ id: reviewId });
    dispatch(setReviews(reviews.filter((review) => review.id !== reviewId)));
  };

  const deleteReview = async (reviewId: number) => {
    await trpc.reviews.delete.mutate({ id: reviewId });
    dispatch(setReviews(reviews.filter((review) => review.id !== reviewId)));
  };

  return (
    <ReviewItemGrid>
      {reviews.length == 0 && <span>There are no unverified reviews</span>}
      {reviews.map((review) => (
        <UnverifiedReview
          key={'verify-' + review.id!}
          review={review}
          onDelete={() => deleteReview(review.id)}
          onVerify={() => verifyReview(review.id)}
        />
      ))}
    </ReviewItemGrid>
  );
};

const Verify: FC = () => {
  const reviews = useAppSelector(selectReviews);
  const [reviewsLoading, setReviewsLoading] = useState(true);
  const dispatch = useAppDispatch();

  const getUnverifiedReviews = useCallback(async () => {
    const res = await trpc.reviews.getAdminView.query({ verified: false });
    dispatch(setReviews(res));
    setReviewsLoading(false);
  }, [dispatch]);

  useEffect(() => {
    getUnverifiedReviews();
    document.title = 'Verify Reviews | PeterPortal';
  }, [getUnverifiedReviews]);

  return (
    <div className="content-wrapper verify-container">
      <h1>Unverified Reviews</h1>
      <p>
        Verifying a review will display the review on top of unverified reviews. Deleting a review will remove it
        permanently.
      </p>
      {reviewsLoading ? <LoadingSpinner /> : <UnverifiedReviewList reviews={reviews} />}
    </div>
  );
};

export default Verify;
