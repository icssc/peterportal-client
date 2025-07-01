import { FC, useCallback, useEffect, useState } from 'react';
import SubReview from '../../component/Review/SubReview';
import { Button } from '@mui/material';
import DeleteForeverIcon from '@mui/icons-material/DeleteForever';
import CheckIcon from '@mui/icons-material/Check';
import './Verify.scss';
import trpc from '../../trpc';
import ReviewGridTemplate from '../ReviewGridTemplate/ReviewGridTemplate';
import { selectReviews, setReviews } from '../../store/slices/reviewSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';

const Verify: FC = () => {
  const reviews = useAppSelector(selectReviews);
  const [reviewsLoading, setReviewsLoading] = useState(true);
  const dispatch = useAppDispatch();

  const getUnverifiedReviews = useCallback(async () => {
    const reviews = await trpc.reviews.getAdminView.query({ verified: false });
    dispatch(setReviews(reviews));
    setReviewsLoading(false);
  }, [dispatch]);

  useEffect(() => {
    getUnverifiedReviews();
    document.title = 'Verify Reviews | PeterPortal';
  }, [getUnverifiedReviews]);

  const verifyReview = async (reviewId: number) => {
    await trpc.reviews.verify.mutate({ id: reviewId });
    dispatch(setReviews(reviews.filter((review) => review.id !== reviewId)));
  };

  const deleteReview = async (reviewId: number) => {
    await trpc.reviews.delete.mutate({ id: reviewId });
    dispatch(setReviews(reviews.filter((review) => review.id !== reviewId)));
  };

  return (
    <ReviewGridTemplate
      title="Unverified Reviews"
      description="Verifying a review will display the review on top of unverified reviews. Deleting a review will remove it permanently."
      isLoading={reviewsLoading}
      noDataMsg="There are currently no unverified reviews."
    >
      {reviews.map((review) => (
        <div key={`verify-${review.id}`}>
          <SubReview review={review}>
            <div className="verification-buttons">
              <Button className="ppc-mui-button" variant="text" onClick={() => deleteReview(review.id)}>
                <DeleteForeverIcon /> Delete
              </Button>
              <Button className="ppc-mui-button primary-button" variant="text" onClick={() => verifyReview(review.id)}>
                <CheckIcon /> Verify
              </Button>
            </div>
          </SubReview>
        </div>
      ))}
    </ReviewGridTemplate>
  );
};

export default Verify;
