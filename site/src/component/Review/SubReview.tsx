import React, { FC, MouseEvent, useState } from 'react';
import axios from 'axios';
import './Review.scss'
import Badge from 'react-bootstrap/Badge';
import { useCookies } from 'react-cookie';

import { ReviewData, VoteRequest } from '../../types/types';

interface SubReviewProps {
  review: ReviewData;
  showCourse: boolean;
}

const SubReview: FC<SubReviewProps> = ({ review, showCourse }) => {
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
      <img className='avatar' src='https://upload.wikimedia.org/wikipedia/commons/thumb/e/e6/Lol_circle.png/479px-Lol_circle.png' />
      <div className='content'>
        <h3 className='identifier'>{showCourse ? review.courseID : (review.professorID.charAt(0).toUpperCase() + review.professorID.slice(1))}</h3>
        <div className='review-info'>
          <p><b>Attendance: </b>{review.attendance ? 'Mandatory' : 'Not Mandatory'}</p>
          <p><b>Would Take Again: </b>{review.takeAgain ? 'Yes' : 'No'}</p>
          <p><b>Textbook: </b>{review.textbook ? 'Yes' : 'No'}</p>
        </div>
        <div className='review-info'>
          <p><b>Quarter Taken: </b>{review.quarter}</p>
          <p><b>Grade Received: </b>{review.gradeReceived}</p>
        </div>
        <div className='review-info'>
          <p><b>Posted by: </b>{review.userDisplay}</p>
          <p><b>Date Posted: </b>{review.timestamp}</p>
        </div>
        <p>{review.reviewContent}</p>
        <div className='review-tags'>
          {review.tags.map((tag, i) =>
            <Badge pill className='p-3 mr-2 mt-2' variant='info' id={`review-tag-${i}`}>
              {tag}
            </Badge>
          )}
        </div>
        <div className='inline' id={review._id}>
          <p>Helpful?</p>
          <button className='upvote' onClick={upvote}>&#9650;</button>
          <p>{score}</p>
          <button className='downvote' onClick={downvote}>&#9660;</button>
          <a href='/report'>Report</a>
        </div>
      </div>
      <div className='ratings'>
        <div className={'r' + Math.floor(review.rating).toString() + ' rating'}><p>{review.rating}</p></div>
        <p><b>QUALITY</b></p>
        <div className={'r' + (6 - Math.floor(review.difficulty)).toString() + ' rating'}><p>{review.difficulty}</p></div>
        <p><b>DIFFICULTY</b></p>
      </div>
    </div>
  )
}

export default SubReview;
