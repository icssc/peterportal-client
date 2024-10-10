import { FC, useContext, useState } from 'react';
import axios from 'axios';
import './Review.scss';
import Badge from 'react-bootstrap/Badge';
import Tooltip from 'react-bootstrap/Tooltip';
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import { useCookies } from 'react-cookie';
import { Link } from 'react-router-dom';
import { PencilFill, PersonFill, TrashFill } from 'react-bootstrap-icons';
import { ReviewData, VoteRequest, CourseGQLData, ProfessorGQLData } from '../../types/types';
import ReportForm from '../ReportForm/ReportForm';
import { selectReviews, setReviews } from '../../store/slices/reviewSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { Button, Modal } from 'react-bootstrap';
import ThemeContext from '../../style/theme-context';
import ReviewForm from '../ReviewForm/ReviewForm';

interface SubReviewProps {
  review: ReviewData;
  course?: CourseGQLData;
  professor?: ProfessorGQLData;
}

const SubReview: FC<SubReviewProps> = ({ review, course, professor }) => {
  const dispatch = useAppDispatch();
  const reviewData = useAppSelector(selectReviews);
  const [cookies] = useCookies(['user']);
  const [reportFormOpen, setReportFormOpen] = useState<boolean>(false);
  const { darkMode } = useContext(ThemeContext);
  const theme = darkMode ? 'dark' : 'light';
  const [showDeleteModal, setShowDeleteModal] = useState(false);
  const [showReviewForm, setShowReviewForm] = useState(false);

  const sendVote = async (voteReq: VoteRequest) => {
    const res = await axios.patch('/api/reviews/vote', voteReq);
    return res.data.deltaScore;
  };

  const updateScore = (newUserVote: number) => {
    dispatch(
      setReviews(
        reviewData.map((otherReview) => {
          if (otherReview._id === review._id) {
            return {
              ...otherReview,
              score: otherReview.score + (newUserVote - otherReview.userVote!),
              userVote: newUserVote,
            };
          } else {
            return otherReview;
          }
        }),
      ),
    );
  };

  const deleteReview = async (reviewID: string) => {
    await axios.delete('/api/reviews', { data: { id: reviewID } });
    dispatch(setReviews(reviewData.filter((review) => review._id !== reviewID)));
    setShowDeleteModal(false);
  };

  const upvote = async () => {
    if (cookies.user === undefined) {
      alert('You must be logged in to vote.');
      return;
    }

    const voteReq = {
      id: review._id!,
      upvote: true,
    };

    let newVote;
    if (review.userVote === 1) {
      newVote = 0;
    } else if (review.userVote === 0) {
      newVote = 1;
    } else {
      newVote = 1;
    }
    updateScore(newVote);
    await sendVote(voteReq).catch((err) => {
      console.error('Error sending upvote:', err);
      updateScore(review.userVote!);
    });
  };

  const downvote = async () => {
    if (cookies.user === undefined) {
      alert('You must be logged in to vote.');
      return;
    }
    const voteReq = {
      id: review._id!,
      upvote: false,
    };

    let newVote;
    if (review.userVote === 1) {
      newVote = -1;
    } else if (review.userVote === 0) {
      newVote = -1;
    } else {
      newVote = 0;
    }
    updateScore(newVote);
    await sendVote(voteReq).catch((err) => {
      console.error('Error sending downvote:', err);
      updateScore(review.userVote!);
    });
  };

  const openReportForm = () => {
    setReportFormOpen(true);
  };

  const openReviewForm = () => {
    setShowReviewForm(true);
    document.body.style.overflow = 'hidden';
  };

  const closeReviewForm = () => {
    setShowReviewForm(false);
    document.body.style.overflow = 'visible';
  };

  const badgeOverlay = <Tooltip id="verified-tooltip">This review was verified by an administrator.</Tooltip>;
  const authorOverlay = <Tooltip id="authored-tooltip">You are the author of this review.</Tooltip>;

  const upvoteClassname = review.userVote === 1 ? 'upvote coloredUpvote' : 'upvote';
  const downvoteClassname = review.userVote === -1 ? 'downvote coloredDownvote' : 'downvote';

  const verifiedBadge = (
    <OverlayTrigger overlay={badgeOverlay}>
      <Badge variant="primary">Verified</Badge>
    </OverlayTrigger>
  );

  const authorBadge = (
    <OverlayTrigger overlay={authorOverlay}>
      <PersonFill size={25} fill="green"></PersonFill>
    </OverlayTrigger>
  );

  return (
    <div className="subreview">
      {cookies.user?.id === review.userID && (
        <div className="edit-buttons">
          <Button variant={theme} className="edit-button" onClick={openReviewForm}>
            <PencilFill width="16" height="16" />
          </Button>
          <Button variant="danger" className="delete-button" onClick={() => setShowDeleteModal(true)}>
            <TrashFill width="16" height="16" />
          </Button>
          <Modal className="ppc-modal" show={showDeleteModal} onHide={() => setShowDeleteModal(false)} centered>
            <Modal.Header closeButton>
              <h2>Delete Review</h2>
            </Modal.Header>
            <Modal.Body>Deleting a review will remove it permanently. Are you sure you want to proceed?</Modal.Body>
            <Modal.Footer>
              <Button variant="secondary" onClick={() => setShowDeleteModal(false)}>
                Cancel
              </Button>
              <Button variant="danger" onClick={() => deleteReview(review._id!)}>
                Delete
              </Button>
            </Modal.Footer>
          </Modal>
        </div>
      )}
      <div>
        <h3 className="subreview-identifier">
          {professor && (
            <Link to={{ pathname: `/course/${review.courseID}` }}>
              {professor.courses[review.courseID]?.department + ' ' + professor.courses[review.courseID]?.courseNumber}
            </Link>
          )}
          {course && (
            <Link to={{ pathname: `/professor/${review.professorID}` }}>
              {course.instructors[review.professorID]?.name}
            </Link>
          )}
          {!course && !professor && (
            <div>
              <Link to={{ pathname: `/course/${review.courseID}` }}>{review.courseID}</Link>{' '}
              <Link to={{ pathname: `/professor/${review.professorID}` }}>{review.professorID}</Link>
            </div>
          )}
        </h3>
      </div>
      <div className="subreview-content">
        <div className="subreview-ratings">
          <div className={'r' + Math.floor(review.rating).toString() + ' rating'}>
            <div className="rating-label">QUALITY</div>
            <div>{review.rating}</div>
          </div>
          <div className={'r' + (6 - Math.floor(review.difficulty)).toString() + ' rating'}>
            <div className="rating-label">DIFFICULTY</div>
            <div>{review.difficulty}</div>
          </div>
        </div>
        <div className="subreview-info">
          <div className="subreview-details">
            <div className="subreview-detail">
              <p>
                Attendance: <b>{review.attendance ? 'Mandatory' : 'Not Mandatory'} </b>
              </p>
              <p>
                Would Take Again: <b>{review.takeAgain ? 'Yes' : 'No'}</b>
              </p>
              <p>
                Textbook: <b>{review.textbook ? 'Yes' : 'No'}</b>
              </p>
            </div>
            <div className="subreview-detail">
              <p>
                Quarter Taken: <b>{review.quarter}</b>
              </p>
              <p>
                Grade Received: <b>{review.gradeReceived}</b>
              </p>
            </div>
          </div>
          <div>
            <div className="subreview-author">
              <p className=" gapped">
                <span className=" mr-1">Posted by {review.userDisplay}</span>
                {review.verified && verifiedBadge}
                {cookies.user?.id === review.userID && authorBadge}
              </p>
              <p>
                {new Date(review.timestamp).toLocaleString('default', {
                  year: 'numeric',
                  month: 'long',
                  day: 'numeric',
                })}
              </p>
            </div>
            <p>{review.reviewContent}</p>
          </div>
        </div>
      </div>
      <div>
        {review.tags?.map((tag) => (
          <Badge pill className="p-3 mr-2 mt-2" variant="info" key={tag}>
            {tag}
          </Badge>
        ))}
      </div>
      <div className="subreview-footer" id={review._id}>
        <p>Helpful?</p>
        <button className={upvoteClassname} onClick={upvote}>
          &#9650;
        </button>
        <p>{review.score}</p>
        <button className={downvoteClassname} onClick={downvote}>
          &#9660;
        </button>
        <button type="button" className="add-report-button" onClick={openReportForm}>
          Report
        </button>
        <ReportForm
          showForm={reportFormOpen}
          reviewID={review._id}
          reviewContent={review.reviewContent}
          closeForm={() => setReportFormOpen(false)}
        />
        <ReviewForm
          course={course}
          professor={professor}
          review={review}
          closeForm={closeReviewForm}
          show={showReviewForm}
          editable={true}
        />
      </div>
    </div>
  );
};

export default SubReview;
