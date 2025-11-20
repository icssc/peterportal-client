'use client';
import './ReviewCard.scss';
import { FC, useState, useEffect, useCallback, ReactNode } from 'react';
import { Chip } from '@mui/material';
import Tooltip from 'react-bootstrap/Tooltip';
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import { CourseGQLData, ProfessorGQLData } from '../../types/types';
import ReportForm from '../ReportForm/ReportForm';
import { selectReviews, setReviews, setToastMsg, setToastSeverity, setShowToast } from '../../store/slices/reviewSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { Modal } from 'react-bootstrap';
import ReviewForm from '../ReviewForm/ReviewForm';
import trpc from '../../trpc';
import { ReviewData } from '@peterportal/types';
import { useIsLoggedIn } from '../../hooks/isLoggedIn';
import { sortTerms } from '../../helpers/util';
import { getProfessorTerms } from '../../helpers/reviews';

import EditIcon from '@mui/icons-material/Edit';
import PersonIcon from '@mui/icons-material/Person';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import { Button, IconButton, Card, Skeleton, Stack } from '@mui/material';
import Link from 'next/link';

interface AuthorEditButtonsProps {
  review: ReviewData;
  course?: CourseGQLData;
  professor?: ProfessorGQLData;
}

const AuthorEditButtons: FC<AuthorEditButtonsProps> = ({ review, course, professor }) => {
  const [showDeleteModal, setShowDeleteModal] = useState(false);
  const [showReviewForm, setShowReviewForm] = useState(false);

  const dispatch = useAppDispatch();
  const reviewData = useAppSelector(selectReviews);

  const sortedTerms: string[] = sortTerms(course?.terms || (professor ? getProfessorTerms(professor) : []));

  const deleteReview = async (reviewId: number) => {
    await trpc.reviews.delete.mutate({ id: reviewId });
    dispatch(setReviews(reviewData.filter((review) => review.id !== reviewId)));
    setShowDeleteModal(false);
  };

  const openReviewForm = () => {
    setShowReviewForm(true);
    document.body.style.overflow = 'hidden';
  };

  const closeReviewForm = () => {
    setShowReviewForm(false);
    document.body.style.overflow = 'visible';
  };

  return (
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
          <Stack direction="row" spacing={2}>
            {' '}
            {/* When the Modal is migrated to MUI, should remove the Stack used for spacing here */}
            <Button color="inherit" onClick={() => setShowDeleteModal(false)}>
              Cancel
            </Button>
            <Button color="error" onClick={() => deleteReview(review.id!)}>
              Delete
            </Button>
          </Stack>
        </Modal.Footer>
      </Modal>
      <ReviewForm
        course={course}
        professor={professor}
        reviewToEdit={review}
        closeForm={closeReviewForm}
        show={showReviewForm}
        editing
        terms={sortedTerms}
      />
    </>
  );
};

interface ReviewCardProps {
  review: ReviewData;
  course?: CourseGQLData;
  professor?: ProfessorGQLData;
  children?: ReactNode;
}

const ReviewCard: FC<ReviewCardProps> = ({ review, course, professor, children }) => {
  const dispatch = useAppDispatch();
  const reviewData = useAppSelector(selectReviews);
  const isLoggedIn = useIsLoggedIn();
  const [identifier, setIdentifier] = useState<ReactNode>(null);
  const [loadingIdentifier, setLoadingIdentifier] = useState<boolean>(true);
  const [reportFormOpen, setReportFormOpen] = useState<boolean>(false);

  const fetchCourseAndProfName = useCallback(async () => {
    let profName: string | undefined = undefined;
    let courseName: string | undefined = undefined;

    try {
      const profResponse = await trpc.professors.get.query({ ucinetid: review.professorId });
      const nameParts = profResponse.name.split(' ');
      const profInitial = nameParts[0][0] + '.';
      const profLastName = nameParts[nameParts.length - 1];
      profName = `${profInitial} ${profLastName}`;

      const matchedCourse = profResponse.courses.find((c) => c.id === review.courseId);

      // first, try to match a course name using the professor's API course array. otherwise, lookup the course separately.
      if (matchedCourse) {
        courseName = `${matchedCourse.department} ${matchedCourse.courseNumber}`;
      } else {
        try {
          const courseResponse = await trpc.courses.get.query({ courseID: review.courseId });
          courseName = `${courseResponse.department} ${courseResponse.courseNumber}`;
        } catch (error) {
          console.error('Error fetching course name: ', error);
        }
      }
      return { courseName, profName };
    } catch (error) {
      console.error('Error fetching professor or course name:', error);
    }
  }, [review.professorId, review.courseId]);

  useEffect(() => {
    const getIdentifier = async () => {
      setLoadingIdentifier(true);
      if (professor) {
        const foundCourse = professor.courses[review.courseId];
        const courseName = foundCourse ? `${foundCourse.department} ${foundCourse.courseNumber}` : review.courseId;
        const courseLink = <Link href={{ pathname: `/course/${review.courseId}` }}>{courseName}</Link>;
        setIdentifier(courseLink);
      } else if (course) {
        const foundProf = course.instructors[review.professorId];
        const profName = foundProf ? `${foundProf.name}` : review.professorId;
        const profLink = <Link href={{ pathname: `/professor/${review.professorId}` }}>{profName}</Link>;
        setIdentifier(profLink);
      } else {
        const foundCourseAndProfName = await fetchCourseAndProfName();
        const courseName = foundCourseAndProfName?.courseName ?? review.courseId;
        const profName = foundCourseAndProfName?.profName ?? review.professorId;
        const courseAndProfLink = (
          <div>
            <Link href={{ pathname: `/course/${review.courseId}` }}>{courseName}</Link>
            {' • '}
            <Link href={{ pathname: `/professor/${review.professorId}` }}>{profName ?? review.professorId}</Link>
          </div>
        );
        setIdentifier(courseAndProfLink);
      }
      setLoadingIdentifier(false);
    };

    getIdentifier();
  }, [course, review.courseId, professor, review.professorId, fetchCourseAndProfName]);

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
      dispatch(setToastMsg('You must be logged in to vote.'));
      dispatch(setToastSeverity('error'));
      dispatch(setShowToast(true));
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

  const badgeOverlay = <Tooltip id="verified-tooltip">This review was verified by an administrator.</Tooltip>;
  const authorOverlay = <Tooltip id="authored-tooltip">You are the author of this review.</Tooltip>;

  const upvoteClassname = review.userVote === 1 ? 'upvote colored-upvote' : 'upvote';
  const downvoteClassname = review.userVote === -1 ? 'downvote colored-downvote' : 'downvote';

  const verifiedBadge = (
    <OverlayTrigger overlay={badgeOverlay}>
      {/* @todo: even smaller variant? */}
      <Chip color="primary" size="small" label="Verified" />
    </OverlayTrigger>
  );

  const authorBadge = (
    <OverlayTrigger overlay={authorOverlay}>
      <Chip color="success" size="small" label={<PersonIcon />} />
    </OverlayTrigger>
  );

  const tags: string[] = review.tags?.slice() ?? [];
  if (review.textbook) tags.unshift('Requires textbook');
  if (review.attendance) tags.unshift('Mandatory attendance');

  return (
    <Card variant="outlined" className="reviewcard">
      <div className="reviewcard-header">
        <h3 className="reviewcard-identifier">
          {loadingIdentifier ? <Skeleton variant="text" animation="wave" width={210} /> : identifier}
        </h3>
        <div className="edit-buttons">
          {!children && review.authored && <AuthorEditButtons review={review} course={course} professor={professor} />}
          {children}
        </div>
      </div>

      <div className="reviewcard-content">
        <div className="reviewcard-ratings">
          <div className={'r' + Math.floor(review.rating).toString() + ' rating'}>
            <div className="rating-label">Quality</div>
            <div className="rating-value">{review.rating}</div>
          </div>
          <div className={'r' + (6 - Math.floor(review.difficulty)).toString() + ' rating'}>
            <div className="rating-label">Difficulty</div>
            <div className="rating-value">{review.difficulty}</div>
          </div>
        </div>
        <div className="reviewcard-info">
          <div className="reviewcard-details">
            <div className="reviewcard-detail">
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
              <div className="reviewcard-author">
                <b>Posted by:</b>
                <p className="reviewcard-author-name">{review.userDisplay}</p>
                {review.verified && <div className="reviewcard-author-verified">{verifiedBadge}</div>}
                {review.authored && <div className="reviewcard-author-author">{authorBadge}</div>}
              </div>
              <p>
                <b>Quarter:</b> {review.quarter}
              </p>
            </div>
            <div className="reviewcard-detail">
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
        <div className="reviewcard-tags">
          {tags.map((tag) => (
            <Chip className="reviewcard-tag" size="small" color="primary" key={tag} label={tag} />
          ))}
        </div>
      )}
      <div className="reviewcard-footer" id={review.id.toString()}>
        <div className="reviewcard-voting">
          <p className="reviewcard-voting-question">Helpful?</p>
          <div className="reviewcard-voting-buttons">
            <button className={upvoteClassname} onClick={upvote}>
              &#9650;
            </button>
            <p className="reviewcard-voting-count">{review.score}</p>
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
      </div>
    </Card>
  );
};

export default ReviewCard;
