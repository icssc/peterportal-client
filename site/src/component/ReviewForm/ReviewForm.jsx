import React, { useState, useEffect } from 'react'
import './ReviewForm.scss'
import axios from 'axios'
import { Icon } from "semantic-ui-react";

const ReviewForm = (props) => {
  const grades = [
    "A+", "A", "A-",
    "B+", "B", "B-",
    "C+", "C", "C-",
    "D+", "D", "D-",
    "F"
  ];

  const [instructors, setInstructors] = useState([])

  const [professor, setProfessor] = useState('');
  const [quarterTaken, setQuarterTaken] = useState('');
  const [gradeReceived, setGradeReceived] = useState('');
  const [content, setContent] = useState("");
  const [quality, setQuality] = useState(null);
  const [difficulty, setDifficulty] = useState(null);

  const [submitted, setSubmitted] = useState(false);
  const [overCharLimit, setOverCharLimit] = useState(false);

  const fetchProfNames = async () => {
    const temp = []
    for (let i=0; i < props.professor_history.length; i+=1) {
      const res = await axios.get(`/professors/api/${props.professor_history[i]}`);
      const prof = {
        name: res.data.name,
        id: props.professor_history[i]
      }
      temp.push(prof)
    }
    setInstructors(temp)
  }

  useEffect(() => {
    fetchProfNames();
  }, [])

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

  const postReview = async (review) => {
    const res = await axios.post('/reviews', review);
  }

  const submitForm = () => {
    const date = new Date();
    const year = date.getFullYear();
    const month = (1 + date.getMonth()).toString();
    const day = date.getDate().toString();
    const review = {
      professorID: professor,
      courseID: props.id,
      userID: 'Anonymous Peter',
      reviewContent: content,
      rating: quality,
      difficulty: difficulty,
      timestamp: month + '/' + day + '/' + year,
      gradeReceived: gradeReceived,
      forCredit: true,
      quarter: quarterTaken,
      score: 0
    };
    if (content.length > 500) {
      setOverCharLimit(true);
    }
    else {
      setOverCharLimit(false);
      postReview(review);
      props.setAddedReview(true);
      setSubmitted(true);
    }
  }

  const ratings = (rating) => (
    <div className="form-ratings">
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
    </div>
  )

  const reviewForm = (
    <>
      <div className="submit-input">
        <label htmlFor="instructor">
          <h5>Taken with:</h5>
          <select name="instructor" id="instructor" onChange={(e) => (setProfessor(document.getElementsByName(e.target.value)[0].id))}>
            <option disabled={true} selected >Instructor</option>
            {instructors.map((instructor, i) => {
              const arr = instructor.name.split(' ');
              const name = `${arr[0][0]}. ${arr[arr.length - 1]}`
              return (
                <option key={i} name={name} id={instructor.id}>{name}</option>
              )
            })}
          </select>
        </label>
        <label htmlFor="quarter">
          <h5>Quarter taken:</h5>
          <select name="quarter" id="quarter"  onChange={(e) => setQuarterTaken(e.target.value)}>
            <option disabled={true} selected >Quarter</option>
            {props.terms.map((quarter, i) => (
              <option key={i}>{quarter}</option>
            ))}
          </select>
        </label>
        <label htmlFor="grade">
          <h5>Grade:</h5>
          <select name="grade" id="grade"  onChange={(e) => setGradeReceived(e.target.value)}>
            <option disabled={true} selected >Grade</option>
            {grades.map((grade, i) => (
              <option key={i}>{grade}</option>
            ))}
          </select>
        </label>
        <textarea rows={15} onChange={(e) => {
          setContent(e.target.value);
          if (overCharLimit && e.target.value.length < 500) {
            setOverCharLimit(false)
          }
          }} />
        <div className="char-limit">
          {overCharLimit ? (<p style={{ color: "red" }}>Your review exceeds the character limit</p>) : null}
          <p style={content.length > 500 ? {color: "red"} : {}} className="chars">{content.length}/500</p>
        </div>
      </div>
      <div className="submit-rating">
        <div>
          <h5>Quality</h5>
          {ratings("q")}
          <h5>Difficulty</h5>
          {ratings("d")}
        </div>
        <button type="button" className="rating-form-btn" onClick={submitForm}>Submit Review</button>
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
