import { FC, useCallback, useEffect, useState } from 'react';
import './UserReviews.scss';

import trpc from '../../trpc';
import SubReview from '../../component/Review/SubReview';
import { selectReviews, setReviews } from '../../store/slices/reviewSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';

const UserReviews: FC = () => {
  const reviews = useAppSelector(selectReviews);
  const [loaded, setLoaded] = useState<boolean>(false);
  const dispatch = useAppDispatch();

  const getUserReviews = useCallback(async () => {
    const response = await trpc.reviews.getUsersReviews.query();
    dispatch(setReviews(response));
    setLoaded(true);
  }, [dispatch]);

  useEffect(() => {
    getUserReviews();
  }, [getUserReviews]);

  if (!loaded) {
    return <p>Loading...</p>;
  }

  if (reviews.length === 0) {
    return <p>No reviews to display at the moment.</p>;
  }

  return (
    <div className="user-reviews-container">
      <h1>Your Reviews</h1>
      <p>Deleting a review will remove it permanently.</p>
      {reviews.map((review) => (
        <div key={review.id!} className="user-reviews">
          <br />
          <SubReview review={review} />
        </div>
      ))}
    </div>
  );
};

export default UserReviews;
