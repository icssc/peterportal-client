import axios, { AxiosResponse } from 'axios';
import { FC, useEffect, useState } from 'react';
import SubReview from '../../component/Review/SubReview';
import { Divider } from 'semantic-ui-react';
import { CourseGQLData, ProfessorGQLData, ReviewData } from '../../../src/types/types';
import './UserReviews.scss';
import { useCookies } from 'react-cookie';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { selectReviews, setFormStatus, setReviews } from '../../store/slices/reviewSlice';
import ReviewForm from '../ReviewForm/ReviewForm';

const UserReviews: FC = () => {
  const reviews = useAppSelector(selectReviews);
  const [loaded, setLoaded] = useState<boolean>(false);
  const [cookies] = useCookies(['user']);
  //edit review states
  const [courseToEdit, setCourseToEdit] = useState<CourseGQLData>();
  const [professorToEdit, setProfessorToEdit] = useState<ProfessorGQLData>();
  const [reviewToEdit, setReviewToEdit] = useState<ReviewData>();
  const dispatch = useAppDispatch();

  const getUserReviews = async () => {
    const response: AxiosResponse<ReviewData[]> = await axios.get(`/api/reviews?userID=${cookies.user.id}`);
    dispatch(setReviews(response.data));
    setLoaded(true);
  };

  useEffect(() => {
    getUserReviews();
  }, []);

  //Edit Review
  const editReview = (review: ReviewData, course?: CourseGQLData, professor?: ProfessorGQLData) => {
    setCourseToEdit(course);
    setProfessorToEdit(professor);
    setReviewToEdit(review);
    dispatch(setFormStatus(true));
    document.body.style.overflow = 'hidden';
  };

  const closeForm = async () => {
    dispatch(setFormStatus(false));
    document.body.style.overflow = 'visible';
    await getUserReviews();
  };

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
          <div key={review._id!} className="user-reviews">
            <Divider />
            <SubReview review={review} editable={true} editReview={editReview} />
          </div>
        ))}
        <ReviewForm
          course={courseToEdit}
          professor={professorToEdit}
          review={reviewToEdit}
          closeForm={closeForm}
          editable={true}
        />
      </div>
    );
  }
};

export default UserReviews;
