import React from 'react';
import { Grid, Image } from "semantic-ui-react";
import "./Review.scss"

const SubReview = ({review}) => {
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
                  <Grid.Column><p><b>Taken With: </b>{review.takenWith}</p></Grid.Column>
                  <Grid.Column textAlign="center"><p><b>Quarter Taken: </b>{review.quarterTaken}</p></Grid.Column>
                  <Grid.Column textAlign="right"><p><b>Grade Received: </b>{review.gradeReceived}</p></Grid.Column>
              </Grid.Row>
              <p>{review.content}</p>
              <Grid.Row columns="equal" style={{ margin: "0" }}>
                  <Grid.Column>
                    <div className="inline">
                      <p>Helpful?</p>
                      <button className="upvote" onClick={() => {console.log("Helpful vote!")}}>&#9650;</button>
                      <p>{review.score}</p>
                      <button className="downvote" onClick={() => {console.log("Unhelpful vote!")}}>&#9660;</button>
                    </div>
                  </Grid.Column>
                  <Grid.Column textAlign="right"><a href="/report">Report</a></Grid.Column>
              </Grid.Row>
          </Grid>
      </Grid.Column>
      <Grid.Column width={2} textAlign="center">
          <div className={"r" + review.quality.toString() + " rating"}><p>{review.quality}</p></div>
          <p><b>QUALITY</b></p>
          <div className={"r" + review.difficulty.toString() + " rating"}><p>{review.difficulty}</p></div>
          <p><b>DIFFICULTY</b></p>
      </Grid.Column>
  </Grid>
  )
}

export default SubReview
