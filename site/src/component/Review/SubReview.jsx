import React, { useState } from 'react';
import axios from 'axios';
import { Grid, Image } from "semantic-ui-react";
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
    <Grid>
      <Grid.Column width={1}  className="avatar">
          <Image src="https://upload.wikimedia.org/wikipedia/commons/thumb/e/e6/Lol_circle.png/479px-Lol_circle.png"/>
      </Grid.Column>
      <Grid.Column width={12}>
          <Grid className="content">
              <h3 className="reviewer">Anonymous Anteater</h3>
              {/* the inline style is to fix the css in CoursePage.scss */}
              <Grid.Row columns="equal" style={{ margin: "0" }}>
                  <Grid.Column><p><b>Taken With: </b>{review.professorID.charAt(0).toUpperCase() + review.professorID.slice(1)}</p></Grid.Column>
                  <Grid.Column textAlign="center"><p><b>Quarter Taken: </b>{review.quarter}</p></Grid.Column>
                  <Grid.Column textAlign="right"><p><b>Grade Received: </b>{review.gradeReceived}</p></Grid.Column>
              </Grid.Row>
              <p>{review.reviewContent}</p>
              <Grid.Row columns="equal" style={{ margin: "0" }}>
                  <Grid.Column>
                    <div className="inline" id={review._id}>
                      <p>Helpful?</p>
                      <button className="upvote" onClick={upvote}>&#9650;</button>
                      <p>{score}</p>
                      <button className="downvote" onClick={downvote}>&#9660;</button>
                    </div>
                  </Grid.Column>
                  <Grid.Column textAlign="right"><a href="/report">Report</a></Grid.Column>
              </Grid.Row>
          </Grid>
      </Grid.Column>
      <Grid.Column width={2} textAlign="center">
          <div className={"r" + Math.floor(review.rating).toString() + " rating"}><p>{review.rating}</p></div>
          <p><b>QUALITY</b></p>
          <div className={"r" + (6-Math.ceil(review.difficulty/2)).toString() + " rating"}><p>{review.difficulty}</p></div>
          <p><b>DIFFICULTY</b></p>
      </Grid.Column>
  </Grid>
  )
}

export default SubReview
