import { FC, useCallback, useEffect, useState } from 'react';
import SubReview from '../../component/Review/SubReview';
import Button from 'react-bootstrap/Button';
import './Verify.scss';
import trpc from '../../trpc';
import { selectReviews, setReviews } from '../../store/slices/reviewSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';

const Verify: FC = () => {
  const reviews = useAppSelector(selectReviews);
  const [loaded, setLoaded] = useState<boolean>(false);
  const dispatch = useAppDispatch();

  const getUnverifiedReviews = useCallback(async () => {
    const res = await trpc.reviews.get.query({ verified: false });
    dispatch(setReviews(res));
    setLoaded(true);
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

  if (!loaded) {
    return <p>Loading...</p>;
  }

  if (reviews.length === 0) {
    return <p>No reviews to display at the moment.</p>;
  }

  return (
    <div className="content-wrapper verify-container">
      <h1>Unverified Reviews</h1>
      <p>Verifying a review will display the review on top of unverified reviews.</p>
      <p>Deleting a review will remove it permanently.</p>
      {reviews.map((review, i) => (
        <div key={`verify-${i}`} className="verify">
          <br />
          <SubReview review={review} />
          <div className="verify-footer">
            <Button variant="danger" className="mr-3" onClick={() => deleteReview(review.id)}>
              Delete
            </Button>
            <Button variant="success" onClick={() => verifyReview(review.id)}>
              Verify
            </Button>
          </div>
        </div>
      ))}
    </div>
  );
};

export default Verify;
