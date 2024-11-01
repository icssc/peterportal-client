import { FC, useCallback, useEffect, useState } from 'react';
import SubReview from '../../component/Review/SubReview';
import { Divider } from 'semantic-ui-react';
import './UserReviews.scss';
import { useCookies } from 'react-cookie';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { selectReviews, setReviews } from '../../store/slices/reviewSlice';
import trpc from '../../trpc';

const UserReviews: FC = () => {
  const reviews = useAppSelector(selectReviews);
  const [loaded, setLoaded] = useState<boolean>(false);
  const [cookies] = useCookies(['user']);
  const dispatch = useAppDispatch();

  const getUserReviews = useCallback(async () => {
    const response = await trpc.reviews.get.query({ userId: cookies.user.id });
    dispatch(setReviews(response));
    setLoaded(true);
  }, [cookies.user.id, dispatch]);

  useEffect(() => {
    getUserReviews();
  }, [getUserReviews]);

  if (!loaded) {
    return <p>Loading...</p>;
  } else if (reviews.length === 0) {
    return <p>No reviews to display at the moment.</p>;
  } else {
    return (
      <div className="user-reviews-container">
        <h1>Your Reviews</h1>
        <p>Deleting a review will remove it permanently.</p>
        {reviews.map((review) => (
          <div key={review.id!} className="user-reviews">
            <Divider />
            <SubReview review={review} />
          </div>
        ))}
      </div>
    );
  }
};

export default UserReviews;
