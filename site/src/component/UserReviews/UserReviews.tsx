import axios, { AxiosResponse } from 'axios';
import { FC, useEffect, useState } from 'react';
import SubReview from '../../component/Review/SubReview';
import Button from 'react-bootstrap/Button';
import { Divider } from 'semantic-ui-react';
import { CourseGQLData, ProfessorGQLData, ReviewData } from '../../../src/types/types';
import './UserReviews.scss';
import { useCookies } from 'react-cookie';
import { useAppDispatch } from '../../store/hooks';
import { setFormStatus } from '../../store/slices/reviewSlice';
import ReviewForm from '../ReviewForm/ReviewForm';
import Modal from 'react-bootstrap/Modal';

const UserReviews: FC = () => {
  const [reviews, setReviews] = useState<ReviewData[]>([]);
  const [loaded, setLoaded] = useState<boolean>(false);
  const [cookies] = useCookies(['user']);
  //edit review states
  const [professorData] = useState<Map<string, ProfessorGQLData>>(new Map());
  const [courseData] = useState<Map<string, CourseGQLData>>(new Map());
  const [courseToEdit, setCourseToEdit] = useState<CourseGQLData>();
  const [professorToEdit, setProfessorToEdit] = useState<ProfessorGQLData>();
  const [reviewToEdit, setReviewToEdit] = useState<ReviewData>();
  const dispatch = useAppDispatch();
  //delete review
  const [show, setShow] = useState(false);
  const handleClose = () => setShow(false);
  const handleShow = () => setShow(true);

  const getUserReviews = async () => {
    const response: AxiosResponse<ReviewData[]> = await axios.get(`/api/reviews?userID=${cookies.user.id}`);
    setReviews(response.data);
    setLoaded(true);
  };

  useEffect(() => {
    getUserReviews();
  }, []);

  //Delete Review
  const deleteReview = async (reviewID: string) => {
    await axios.delete('/api/reviews', { data: { id: reviewID } });
    setReviews(reviews.filter((review) => review._id !== reviewID));
    handleClose();
  };

  //Edit Review
  const editReview = (review: ReviewData, course?: CourseGQLData, professor?: ProfessorGQLData) => {
    setCourseToEdit(course);
    setProfessorToEdit(professor);
    setReviewToEdit(review);
    console.log('Data received from SubReview:', {
      course,
      professor,
      review,
    });
    dispatch(setFormStatus(true));
    document.body.style.overflow = 'hidden';
    console.log('Edit Review clicked!');
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
        {reviews.map((review, i) => (
          <div key={`user-reviews-${i}`} className="user-reviews">
            <Divider />
            <SubReview
              review={review}
              course={courseData.get(review.courseID)}
              professor={professorData.get(review.professorID)}
              editable={true}
              editReview={editReview}
            />
            <div className="user-reviews-footer">
              <div className="delete-review-dialog">
                <Button variant="danger" className="mr-3" onClick={handleShow}>
                  Delete
                </Button>
                <Modal show={show} onHide={handleClose} backdrop="static" keyboard={true} centered autoFocus>
                  <Modal.Header closeButton>
                    <Modal.Title>Delete Review Confirmation</Modal.Title>
                  </Modal.Header>
                  <Modal.Body>
                    Deleting a review will remove it permanently. Are you sure you want to proceed?
                  </Modal.Body>
                  <Modal.Footer>
                    <Button variant="secondary" onClick={handleClose}>
                      Close
                    </Button>
                    <Button variant="danger" onClick={() => deleteReview(review._id!)}>
                      Delete
                    </Button>
                  </Modal.Footer>
                </Modal>
              </div>
            </div>
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
