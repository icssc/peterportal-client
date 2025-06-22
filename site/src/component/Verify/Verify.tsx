import { FC, useCallback, useEffect, useState } from 'react';
import SubReview from '../../component/Review/SubReview';
import { Button } from '@mui/material';
import DeleteForeverIcon from '@mui/icons-material/Delete';
import CheckIcon from '@mui/icons-material/Check';
import './Verify.scss';
import trpc from '../../trpc';
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

  // TODO: class for user reviews container (move to its own component?)
  return (
    <div className="user-reviews-container">
      {reviews.length == 0 && <span>There are no unverified reviews</span>}
      {reviews.map((review) => (
        <div key={'verify-' + review.id!} className="user-review-wrapper">
          <SubReview review={review}>
            <Button className="delete-button" variant="contained" onClick={() => deleteReview(review.id)}>
              <DeleteForeverIcon /> Delete
            </Button>
            <Button className="verify-button" variant="contained" onClick={() => verifyReview(review.id)}>
              <CheckIcon /> Verify
            </Button>
          </SubReview>
        </div>
      ))}
    </div>
  );
};

const Verify: FC = () => {
  const reviews = useAppSelector(selectReviews);
  const [loaded, setLoaded] = useState<boolean>(false);
  const dispatch = useAppDispatch();

  const getUnverifiedReviews = useCallback(async () => {
    const res = await trpc.reviews.getAdminView.query({ verified: false });
    dispatch(setReviews(res));
    setLoaded(true);
  }, [dispatch]);

  useEffect(() => {
    getUnverifiedReviews();
    document.title = 'Verify Reviews | PeterPortal';
  }, [getUnverifiedReviews]);

  if (!loaded) {
    return <p>Loading...</p>;
  } else if (reviews.length === 0) {
    return <p>No reviews to display at the moment.</p>;
  } else {
    return (
      <div className="content-wrapper verify-container">
        <h1>Unverified Reviews</h1>
        <p>
          Verifying a review will display the review on top of unverified reviews. Deleting a review will remove it
          permanently.
        </p>
        <UnverifiedReviewsList reviews={reviews} />
      </div>
    );
  }
};

export default Verify;
