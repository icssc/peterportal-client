import React, { useState } from 'react';
import axios from 'axios';
import "./Review.scss"

const SubReview = ({review}) => {
  const [score, setScore] = useState(review.score);

  const voteReq = async (vote) => {
    const res = await axios.patch('/reviews/vote', vote);
  }

  const upvote = (e) => {
    const votes = {
      id: e.target.parentNode.id,
      upvote: true
    }
    voteReq(votes)
    setScore(score+1)
  }

  const downvote = (e) => {
    const votes = {
      id: e.target.parentNode.id,
      upvote: false
    }
    voteReq(votes)
    setScore(score-1)
  }

  return (
    <div className="subreview">
      <img className="avatar" src="https://upload.wikimedia.org/wikipedia/commons/thumb/e/e6/Lol_circle.png/479px-Lol_circle.png" />
      <div className="content">
        <h3 className="reviewer">{review.userID}</h3>
        <div className="review-info">
          <p><b>Taken With: </b>{review.professorID.charAt(0).toUpperCase() + review.professorID.slice(1)}</p>
          <p><b>Quarter Taken: </b>{review.quarter}</p>
          <p><b>Grade Received: </b>{review.gradeReceived}</p>
        </div>
        <p>{review.reviewContent}</p>
        <div className="inline" id={review._id}>
          <p>Helpful?</p>
          <button className="upvote" onClick={upvote}>&#9650;</button>
          <p>{score}</p>
          <button className="downvote" onClick={downvote}>&#9660;</button>
          <a href="/report">Report</a>
        </div>
        
      </div>
      <div className="ratings">
        <div className={"r" + Math.floor(review.rating).toString() + " rating"}><p>{review.rating}</p></div>
        <p><b>QUALITY</b></p>
        <div className={"r" + (6-Math.floor(review.difficulty)).toString() + " rating"}><p>{review.difficulty}</p></div>
        <p><b>DIFFICULTY</b></p>
      </div>
    </div>
  )
}

export default SubReview
