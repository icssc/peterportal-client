import { FC, useEffect, useState } from 'react';
import SubReview from '../../component/Review/SubReview';
import Button from 'react-bootstrap/Button';
import { Divider } from 'semantic-ui-react';
import './Verify.scss';
import trpc from '../../trpc';
import { ReviewData } from '@peterportal/types';

const Verify: FC = () => {
  const [reviews, setReviews] = useState<ReviewData[]>([]);
  const [loaded, setLoaded] = useState<boolean>(false);

  const getUnverifiedReviews = async () => {
    const res = await trpc.reviews.get.query({ verified: false });
    setReviews(res);
    setLoaded(true);
  };

  useEffect(() => {
    getUnverifiedReviews();
    document.title = 'Verify Reviews | PeterPortal';
  }, []);

  const verifyReview = async (reviewId: number) => {
    await trpc.reviews.verify.mutate({ id: reviewId });
    setReviews(reviews.filter((review) => review.id !== reviewId));
  };

  const deleteReview = async (reviewId: number) => {
    await trpc.reviews.delete.mutate({ id: reviewId });
    setReviews(reviews.filter((review) => review.id !== reviewId));
  };

  if (!loaded) {
    return <p>Loading...</p>;
  } else if (reviews.length === 0) {
    return <p>No reviews to display at the moment.</p>;
  } else {
    return (
      <div className="verify-container">
        <h1>Unverified Reviews</h1>
        <p>Verifying a review will display the review on top of unverified reviews.</p>
        <p>Deleting a review will remove it permanently.</p>
        {reviews.map((review) => (
          <div key={review.id} className="verify">
            <Divider />
            <SubReview review={review}></SubReview>
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
  }
};

export default Verify;
