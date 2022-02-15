import React, { FC, MouseEvent, useState } from 'react';
import axios from 'axios';
import './Review.scss'
import Badge from 'react-bootstrap/Badge';
import { useCookies } from 'react-cookie';
import { Link } from 'react-router-dom';

import { ReviewData, VoteRequest, CourseGQLData, ProfessorGQLData } from '../../types/types';

interface SubReviewProps {
  review: ReviewData;
  course?: CourseGQLData;
  professor?: ProfessorGQLData;
}

const SubReview: FC<SubReviewProps> = ({ review, course, professor }) => {
  const [score, setScore] = useState(review.score);
  const [cookies, setCookie] = useCookies(['user']);

  const voteReq = async (vote: VoteRequest) => {
    const res = await axios.patch('/reviews/vote', vote);
  }

  const upvote = (e: MouseEvent) => {
    if (!cookies.hasOwnProperty('user')) {
      alert('You must be logged in to vote.');
      return;
    }
    const votes = {
      id: ((e.target as HTMLElement).parentNode! as Element).getAttribute('id')!,
      upvote: true
    }
    voteReq(votes)
    setScore(score + 1)
  }

  const downvote = (e: MouseEvent) => {
    if (!cookies.hasOwnProperty('user')) {
      alert('You must be logged in to vote.');
      return;
    }
    const votes = {
      id: ((e.target as HTMLElement).parentNode! as Element).getAttribute('id')!,
      upvote: false
    }
    voteReq(votes)
    setScore(score - 1)
  }

  return (
    <div className='subreview'>
      <div>
        <h3 className='subreview-identifier'>
          {professor && <Link to={{ pathname: `/course/${review.courseID}` }}>
            {professor.course_history[review.courseID].department + ' ' + professor.course_history[review.courseID].number}
          </Link>}
          {course && <Link to={{ pathname: `/professor/${review.professorID}` }}>
            {course.instructor_history[review.professorID].name}
          </Link>}
        </h3>
      </div>
      <div className='subreview-content'>
        <div className='subreview-ratings'>
          <div className={'r' + Math.floor(review.rating).toString() + ' rating'}>
            <div className='rating-label'>QUALITY</div>
            <div>{review.rating}</div>
          </div>
          <div className={'r' + (6 - Math.floor(review.difficulty)).toString() + ' rating'}>
            <div className='rating-label'>DIFFICULTY</div>
            <div>{review.difficulty}</div>
          </div>
        </div>
        <div className='subreview-info'>
          <div className='subreview-details'>
            <div className='subreview-detail'>
              <p>Attendance: <b>{review.attendance ? 'Mandatory' : 'Not Mandatory'} </b></p>
              <p>Would Take Again: <b>{review.takeAgain ? 'Yes' : 'No'}</b></p>
              <p>Textbook: <b>{review.textbook ? 'Yes' : 'No'}</b></p>
            </div>
            <div className='subreview-detail'>
              <p>Quarter Taken: <b>{review.quarter}</b></p>
              <p>Grade Received: <b>{review.gradeReceived}</b></p>
            </div>
          </div>
          <div>
            <div className='subreview-author'>
              <p>Posted by {review.userDisplay}</p>
              <p>{new Date(review.timestamp).toLocaleString('default', { year: 'numeric', month: 'long', day: 'numeric' })}</p>
            </div>
            <p>{review.reviewContent}</p>
          </div>
        </div>
      </div>
      <div>
        {review.tags?.map((tag, i) =>
          <Badge pill className='p-3 mr-2 mt-2' variant='info' key={`review-tag-${review._id}-${i}`}>
            {tag}
          </Badge>
        )}
      </div>
      <div className='subreview-footer' id={review._id}>
        <p>Helpful?</p>
        <button className='upvote' onClick={upvote}>&#9650;</button>
        <p>{score}</p>
        <button className='downvote' onClick={downvote}>&#9660;</button>
        <a href='/report'>Report</a>
      </div>
    </div>
  )
}

export default SubReview;
