import { FC, useState, ReactNode } from 'react';
import './SubReview.scss';
import Badge from 'react-bootstrap/Badge';
import Tooltip from 'react-bootstrap/Tooltip';
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import { Link } from 'react-router-dom';
import { CourseGQLData, ProfessorGQLData } from '../../types/types';
import ReportForm from '../ReportForm/ReportForm';
import { selectReviews, setReviews } from '../../store/slices/reviewSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { Button, Modal } from 'react-bootstrap';
import ReviewForm from '../ReviewForm/ReviewForm';
import trpc from '../../trpc';
import { ReviewData } from '@peterportal/types';
import { useIsLoggedIn } from '../../hooks/isLoggedIn';
import spawnToast from '../../helpers/toastify';
import { sortTerms } from '../../helpers/util';
import { getProfessorTerms } from '../../helpers/reviews';

import EditIcon from '@mui/icons-material/Edit';
import PersonIcon from '@mui/icons-material/Person';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import { IconButton, Paper } from '@mui/material';

interface SubReviewProps {
  review: ReviewData;
  course?: CourseGQLData;
  professor?: ProfessorGQLData;
  children?: ReactNode;
}

const SubReview: FC<SubReviewProps> = ({ review, course, professor, children }) => {
  const dispatch = useAppDispatch();
  const reviewData = useAppSelector(selectReviews);
  const isLoggedIn = useIsLoggedIn();
  const [reportFormOpen, setReportFormOpen] = useState<boolean>(false);
  const [showDeleteModal, setShowDeleteModal] = useState(false);
  const [showReviewForm, setShowReviewForm] = useState(false);

  const updateScore = (newUserVote: number) => {
    dispatch(
      setReviews(
        reviewData.map((otherReview) => {
          if (otherReview.id === review.id) {
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

  const deleteReview = async (reviewId: number) => {
    await trpc.reviews.delete.mutate({ id: reviewId });
    dispatch(setReviews(reviewData.filter((review) => review.id !== reviewId)));
    setShowDeleteModal(false);
  };

  const upvote = async () => {
    const newVote = review.userVote === 1 ? 0 : 1;
    await vote(newVote);
  };

  const downvote = async () => {
    const newVote = review.userVote === -1 ? 0 : -1;
    await vote(newVote);
  };

  const vote = async (newVote: number) => {
    if (!isLoggedIn) {
      spawnToast('You must be logged in to vote.', true);
      return;
    }
    updateScore(newVote);
    try {
      await trpc.reviews.vote.mutate({ id: review.id, vote: newVote });
    } catch (err) {
      updateScore(review.userVote);
      console.error('Error sending downvote:', err);
    }
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

  const upvoteClassname = review.userVote === 1 ? 'upvote colored-upvote' : 'upvote';
  const downvoteClassname = review.userVote === -1 ? 'downvote colored-downvote' : 'downvote';

  const verifiedBadge = (
    <OverlayTrigger overlay={badgeOverlay}>
      <Badge variant="primary">Verified</Badge>
    </OverlayTrigger>
  );

  const authorBadge = (
    <OverlayTrigger overlay={authorOverlay}>
      <Badge variant="success" style={{ padding: '1px' }}>
        <PersonIcon />
      </Badge>
    </OverlayTrigger>
  );

  const tags: string[] = review.tags?.slice() ?? [];
  if (review.textbook) tags.unshift('Requires textbook');
  if (review.attendance) tags.unshift('Mandatory attendance');

  const sortedTerms: string[] = sortTerms(course?.terms || (professor ? getProfessorTerms(professor) : []));

  return (
    <Paper className="subreview ppc-paper">
      <div className="subreview-header">
        <h3 className="subreview-identifier">
          {professor && (
            <Link to={{ pathname: `/course/${review.courseId}` }}>
              {professor.courses[review.courseId]
                ? professor.courses[review.courseId]?.department +
                  ' ' +
                  professor.courses[review.courseId]?.courseNumber
                : review.courseId}
            </Link>
          )}
          {course && (
            <Link to={{ pathname: `/professor/${review.professorId}` }}>
              {course.instructors[review.professorId]?.name ?? review.professorId}
            </Link>
          )}
          {!course && !professor && (
            <div>
              <Link to={{ pathname: `/course/${review.courseId}` }}>{review.courseId}</Link>{' '}
              <Link to={{ pathname: `/professor/${review.professorId}` }}>{review.professorId}</Link>
            </div>
          )}
        </h3>
        <div className="edit-buttons">
          {review.authored && (
            <>
              <IconButton onClick={openReviewForm}>
                <EditIcon />
              </IconButton>
              <IconButton onClick={() => setShowDeleteModal(true)}>
                <DeleteOutlineIcon />
              </IconButton>
              <Modal className="ppc-modal" show={showDeleteModal} onHide={() => setShowDeleteModal(false)} centered>
                <Modal.Header closeButton>
                  <h2>Delete Review</h2>
                </Modal.Header>
                <Modal.Body>Deleting a review will remove it permanently. Are you sure you want to proceed?</Modal.Body>
                <Modal.Footer>
                  <Button variant="secondary" onClick={() => setShowDeleteModal(false)}>
                    Cancel
                  </Button>
                  <Button variant="danger" onClick={() => deleteReview(review.id!)}>
                    Delete
                  </Button>
                </Modal.Footer>
              </Modal>
            </>
          )}
          {children}
        </div>
      </div>

      <div className="subreview-content">
        <div className="subreview-ratings">
          <div className={'r' + Math.floor(review.rating).toString() + ' rating'}>
            <div className="rating-label">Quality</div>
            <div className="rating-value">{review.rating}</div>
          </div>
          <div className={'r' + (6 - Math.floor(review.difficulty)).toString() + ' rating'}>
            <div className="rating-label">Difficulty</div>
            <div className="rating-value">{review.difficulty}</div>
          </div>
        </div>
        <div className="subreview-info">
          <div className="subreview-details">
            <div className="subreview-detail">
              <p>
                <b>Posted on:</b>
                {' ' +
                  new Date(review.createdAt).toLocaleString('default', {
                    year: 'numeric',
                    month: 'long',
                    day: 'numeric',
                  })}
                {review.updatedAt && (
                  <span className="subtext edit-time"> (edited {new Date().toLocaleDateString()})</span>
                )}
              </p>
              <div className="subreview-author">
                <b>Posted by:</b>
                <p className="subreview-author-name">{review.userDisplay}</p>
                {review.verified && <div className="subreview-author-verified">{verifiedBadge}</div>}
                {review.authored && <div className="subreview-author-author">{authorBadge}</div>}
              </div>
              <p>
                <b>Quarter:</b> {review.quarter}
              </p>
            </div>
            <div className="subreview-detail">
              <p>
                <b>Grade:</b> {review.gradeReceived}
              </p>
              <p>
                <b>Would Take Again:</b> {review.takeAgain ? 'Yes' : 'No'}
              </p>
            </div>
          </div>
          <p className="review-content">{review.content || <i>This review has no additional content</i>}</p>
        </div>
      </div>
      {tags.length > 0 && (
        <div className="subreview-tags">
          {tags.map((tag) => (
            <Badge pill className="subreview-tag" key={tag}>
              {tag}
            </Badge>
          ))}
        </div>
      )}
      <div className="subreview-footer" id={review.id.toString()}>
        <div className="subreview-voting">
          <p className="subreview-voting-question">Helpful?</p>
          <div className="subreview-voting-buttons">
            <button className={upvoteClassname} onClick={upvote}>
              &#9650;
            </button>
            <p className="subreview-voting-count">{review.score}</p>
            <button className={downvoteClassname} onClick={downvote}>
              &#9660;
            </button>
          </div>
        </div>
        <button className="add-report-button" onClick={openReportForm}>
          Report...
        </button>
        <ReportForm
          showForm={reportFormOpen}
          reviewId={review.id}
          reviewContent={review.content}
          closeForm={() => setReportFormOpen(false)}
        />
        <ReviewForm
          course={course}
          professor={professor}
          reviewToEdit={review}
          closeForm={closeReviewForm}
          show={showReviewForm}
          editing
          terms={sortedTerms}
        />
      </div>
    </Paper>
  );
};

export default SubReview;
