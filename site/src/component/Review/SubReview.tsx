import React, { FC, MouseEvent, useState, useEffect } from 'react';
import axios from 'axios';
import './Review.scss';
import Badge from 'react-bootstrap/Badge';
import Tooltip from 'react-bootstrap/Tooltip';
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import { useCookies } from 'react-cookie';
import { Link } from 'react-router-dom';

import {
  ReviewData,
  VoteRequest,
  CourseGQLData,
  ProfessorGQLData,
  VoteColorRequest,
  VoteColor,
} from '../../types/types';
import ReportForm from '../ReportForm/ReportForm';
import * as Icon from 'react-bootstrap-icons';
import Modal from 'react-bootstrap/Modal';
import { Button } from 'react-bootstrap';
import ReviewForm from '../ReviewForm/ReviewForm';
import { ReviewProps } from '../Review/Review';
import { propTypes } from 'react-bootstrap/esm/Image';
import { useAppDispatch, useAppSelector } from 'src/store/hooks';
import {
  selectReviews,
  setReviews,
  setFormStatus,
} from '../../store/slices/reviewSlice';
interface SubReviewProps {
  review: ReviewData;
  course?: CourseGQLData;
  professor?: ProfessorGQLData;
  colors?: VoteColor;
  colorUpdater?: () => void;
  editable?: boolean;
  editReview?: (
    review: ReviewData,
    course?: CourseGQLData,
    professor?: ProfessorGQLData
  ) => void;
}

const SubReview: FC<SubReviewProps> = ({
  review,
  course,
  professor,
  colors,
  colorUpdater,
  editable,
  editReview,
}) => {
  const [score, setScore] = useState(review.score);
  const [cookies, setCookie] = useCookies(['user']);
  const [upvoteClassname, setUpvoteClass] = useState('upvote');
  const [downvoteClassname, setDownvoteClass] = useState('downvote');
  //Edit Review
  const [showReviewForm, setShowReviewForm] = useState(true);
  const reviewData = useAppSelector(selectReviews);
  console.log(course);
  console.log(professor);

  let upvoteClass;
  let downvoteClass;
  if (colors != undefined && colors.colors != undefined) {
    upvoteClass = colors.colors[0] ? 'upvote coloredUpvote' : 'upvote';
    downvoteClass = colors.colors[1] ? 'downvote coloredDownvote' : 'downvote';
  } else {
    upvoteClass = 'upvote';
    downvoteClass = 'downvote';
  }
  const [reportFormOpen, setReportFormOpen] = useState<boolean>(false);
  const voteReq = async (vote: VoteRequest) => {
    const res = await axios.patch('/api/reviews/vote', vote);
    return res.data.deltaScore;
  };

  const upvote = async (e: MouseEvent) => {
    if (!cookies.hasOwnProperty('user')) {
      alert('You must be logged in to vote.');
      return;
    }
    const votes = {
      id: ((e.target as HTMLElement).parentNode! as Element).getAttribute(
        'id'
      )!,
      upvote: true,
    };
    let deltaScore = await voteReq(votes);

    setScore(score + deltaScore);
    if (colorUpdater != undefined) {
      colorUpdater();
    }
  };

  const downvote = async (e: MouseEvent) => {
    if (!cookies.hasOwnProperty('user')) {
      alert('You must be logged in to vote.');
      return;
    }
    const votes = {
      id: ((e.target as HTMLElement).parentNode! as Element).getAttribute(
        'id'
      )!,
      upvote: false,
    };
    let deltaScore = await voteReq(votes);
    setScore(score + deltaScore);
    if (colorUpdater != undefined) {
      colorUpdater();
    }
  };

  const openReportForm = (e: MouseEvent) => {
    setReportFormOpen(true);
  };

  const badgeOverlay = (
    <Tooltip id='verified-tooltip'>
      This review was verified by an administrator.
    </Tooltip>
  );

  const verifiedBadge = (
    <OverlayTrigger overlay={badgeOverlay}>
      <Badge variant='primary'>Verified</Badge>
    </OverlayTrigger>
  );

  return (
    <div className='subreview'>
      {editable && editReview && (
        <div style={{ float: 'right', width: '3%', height: '3%' }}>
          <Icon.PenFill onClick={() => editReview(review, course, professor)} />
        </div>
      )}
      <div>
        <h3 className='subreview-identifier'>
          {professor && (
            <Link to={{ pathname: `/course/${review.courseID}` }}>
              {professor.course_history[review.courseID].department +
                ' ' +
                professor.course_history[review.courseID].number}
            </Link>
          )}
          {course && (
            <Link to={{ pathname: `/professor/${review.professorID}` }}>
              {course.instructor_history[review.professorID]?.name}
            </Link>
          )}
          {!course && !professor && (
            <div>
              {review.courseID} {review.professorID}
            </div>
          )}
        </h3>
      </div>
      <div className='subreview-content'>
        <div className='subreview-ratings'>
          <div
            className={'r' + Math.floor(review.rating).toString() + ' rating'}
          >
            <div className='rating-label'>QUALITY</div>
            <div>{review.rating}</div>
          </div>
          <div
            className={
              'r' + (6 - Math.floor(review.difficulty)).toString() + ' rating'
            }
          >
            <div className='rating-label'>DIFFICULTY</div>
            <div>{review.difficulty}</div>
          </div>
        </div>
        <div className='subreview-info'>
          <div className='subreview-details'>
            <div className='subreview-detail'>
              <p>
                Attendance:{' '}
                <b>{review.attendance ? 'Mandatory' : 'Not Mandatory'} </b>
              </p>
              <p>
                Would Take Again: <b>{review.takeAgain ? 'Yes' : 'No'}</b>
              </p>
              <p>
                Textbook: <b>{review.textbook ? 'Yes' : 'No'}</b>
              </p>
            </div>
            <div className='subreview-detail'>
              <p>
                Quarter Taken: <b>{review.quarter}</b>
              </p>
              <p>
                Grade Received: <b>{review.gradeReceived}</b>
              </p>
            </div>
          </div>
          <div>
            <div className='subreview-author'>
              <p>
                <span className='mr-1'>Posted by {review.userDisplay}</span>
                {review.verified && verifiedBadge}
              </p>
              <p>
                {new Date(review.timestamp).toLocaleString('default', {
                  year: 'numeric',
                  month: 'long',
                  day: 'numeric',
                })}
              </p>
            </div>
            <div>
              <p>{review.reviewContent}</p>
            </div>
          </div>
        </div>
      </div>
      <div>
        {review.tags?.map((tag, i) => (
          <Badge
            pill
            className='p-3 mr-2 mt-2'
            variant='info'
            key={`review-tag-${review._id}-${i}`}
          >
            {tag}
          </Badge>
        ))}
      </div>
      <div className='subreview-footer' id={review._id}>
        <p>Helpful?</p>
        <button className={upvoteClass} onClick={upvote}>
          &#9650;
        </button>
        <p>{score}</p>
        <button className={downvoteClass} onClick={downvote}>
          &#9660;
        </button>
        <button
          type='button'
          className='add-report-button'
          onClick={openReportForm}
        >
          Report
        </button>
        <ReportForm
          showForm={reportFormOpen}
          reviewID={review._id}
          reviewContent={review.reviewContent}
          closeForm={() => setReportFormOpen(false)}
        />
      </div>
    </div>
  );
};

export default SubReview;
