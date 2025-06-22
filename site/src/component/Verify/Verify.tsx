import { FC, useCallback, useEffect, useState } from 'react';
import SubReview from '../../component/Review/SubReview';
import { Button } from '@mui/material';
import DeleteForeverIcon from '@mui/icons-material/DeleteForever';
import CheckIcon from '@mui/icons-material/Check';
import './Verify.scss';
import trpc from '../../trpc';
import ReviewItemGrid from '../../component/ReviewItemGrid/ReviewItemGrid';
import { selectReviews, setReviews } from '../../store/slices/reviewSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { ReviewData } from '@peterportal/types';

const UnverifiedReviewsList: FC<{ reviews: ReviewData[] }> = ({ reviews }) => {
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
        <div key={'verify-' + review.id!}>
          <SubReview review={review}>
            <div className="verification-buttons">
              <Button className="ppc-mui-button with-icon" variant="contained" onClick={() => deleteReview(review.id)}>
                <DeleteForeverIcon /> Delete
              </Button>
              <Button
                className="ppc-mui-button primary-button with-icon"
                variant="contained"
                onClick={() => verifyReview(review.id)}
              >
                <CheckIcon /> Verify
              </Button>
            </div>
          </SubReview>
        </div>
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

  /** @todo replace the loading text here with LoadingSpinner once that gets merged */
  return (
    <div className="content-wrapper verify-container">
      <h1>Unverified Reviews</h1>
      <p>
        Verifying a review will display the review on top of unverified reviews. Deleting a review will remove it
        permanently.
      </p>
      {reviewsLoading ? <p>Loading...</p> : <UnverifiedReviewsList reviews={reviews} />}
    </div>
  );
};

export default Verify;
