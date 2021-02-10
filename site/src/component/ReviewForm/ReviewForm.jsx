import React, { useState } from 'react'
import './ReviewForm.scss'
import { Icon } from "semantic-ui-react";

const DUMMY_DATA = {
  "instructors": [
    "R. Pattis",
    "M. Shindler",
    "A. Thornton"
  ],
  "quarters": [
    "Winter 2020",
    "Fall 2020",
    "Spring 2020"
  ],
  "grades": [
    "A+", "A", "A-",
    "B+", "B", "B-",
    "C+", "C", "C-",
    "D+", "D", "D-",
    "F"
  ]
};

const ReviewForm = () => {
  const [instructors, setInstructors] = useState(DUMMY_DATA.instructors);
  const [quarters, setQuarters] = useState(DUMMY_DATA.quarters);
  const [grades, setGrades] = useState(DUMMY_DATA.grades);

  const [professor, setProfessor] = useState('');
  const [quarterTaken, setQuarterTaken] = useState('');
  const [gradeReceived, setGradeReceived] = useState('');
  const [content, setContent] = useState("");
  const [quality, setQuality] = useState(null);
  const [difficulty, setDifficulty] = useState(null);

  const [submitted, setSubmitted] = useState(false);

  const reviewRate = (e) => {
    const rating = document.getElementById(e.target.id).nextElementSibling;
    const ratings = document.getElementsByName(e.target.name)
    for (let i=0; i < ratings.length; i+=1) {
      ratings[i].nextElementSibling.classList.remove('active-rating');
    }
    rating.classList.add('active-rating');
    
    if (e.target.name === 'q') {
      setQuality(parseInt(e.target.id[1]));
    } else if (e.target.name === 'd') {
      setDifficulty(parseInt(e.target.id[1]));
    }
  }

  const submitReview = () => {
    const review = {
      professorID: professor,
      courseID: '',
      userID: 'Anonymous Peter',
      reviewContent: content,
      rating: quality,
      difficulty: difficulty,
      timestamp: new Date(),
      gradeReceived: gradeReceived,
      forCredit: true,
      quarter: quarterTaken,
      score: 0
    };
    console.log(review);
    setSubmitted(true);
  }

  const ratings = (rating) => (
    <>
      <input type="radio" name={rating} id={rating+"1"} onChange={reviewRate}/>
      <label htmlFor={rating+"1"} className="r1">1</label>
      <input type="radio" name={rating} id={rating+"2"} onChange={reviewRate}/>
      <label htmlFor={rating+"2"} className="r2">2</label>
      <input type="radio" name={rating} id={rating+"3"} onChange={reviewRate}/>
      <label htmlFor={rating+"3"} className="r3">3</label>
      <input type="radio" name={rating} id={rating+"4"} onChange={reviewRate}/>
      <label htmlFor={rating+"4"} className="r4">4</label>
      <input type="radio" name={rating} id={rating+"5"} onChange={reviewRate}/>
      <label htmlFor={rating+"5"} className="r5">5</label>
    </>
  )

  const reviewForm = (
    <>
      <div className="submit-input">
        <label for="instructor">
          <h4>Taken with:</h4>
          <select name="instructor" id="instructor" onChange={(e) => setProfessor(e.target.value)}>
            <option disabled={true} selected="selected" >Instructor</option>
            {instructors.map((instructor) => (
              <option>{instructor}</option>
            ))}
          </select>
        </label>
        <label for="quarter">
          <h4>Quarter taken:</h4>
          <select name="quarter" id="quarter"  onChange={(e) => setQuarterTaken(e.target.value)}>
            <option disabled={true} selected="selected" >Quarter</option>
            {quarters.map((quarter) => (
              <option>{quarter}</option>
            ))}
          </select>
        </label>
        <label for="grade">
          <h4>Grade:</h4>
          <select name="grade" id="grade"  onChange={(e) => setGradeReceived(e.target.value)}>
            <option disabled={true} selected="selected" >Grade</option>
            {grades.map((grade) => (
              <option>{grade}</option>
            ))}
          </select>
        </label>
        <textarea rows={15} onChange={(e) => setContent(e.target.value)} />
        <p style={content.length > 500 ? {color: "red"} : {}}>{content.length}/500</p>
      </div>
      <div className="submit-rating">
        <div>
          <h4>Quality</h4>
          {ratings("q")}
          <h4>Difficulty</h4>
          {ratings("d")}
        </div>
        <button type="button" className="rating-form-btn" onClick={submitReview}>Submit Review</button>
      </div>
    </>
  )

  return (
    <div className="submit-review">
      {submitted ? (
        <div className="submitted-form">
          <Icon name="check circle" size="huge"/>
          <h1>Thank You</h1>
          <p>Your form has been submitted successfully.</p>
        </div>
      ) : reviewForm}
    </div>
  )
}

export default ReviewForm
