import React, { FC, ChangeEvent, useState, useEffect } from 'react'
import './ReviewForm.scss'
import axios from 'axios'
import { useCookies } from 'react-cookie';
import { Icon } from 'semantic-ui-react';
import { useQuery } from '@apollo/client';
import { useProfessorNames } from '../../hooks/professorNames';

import { addReview } from '../../store/slices/reviewSlice';
import { useAppDispatch } from '../../store/hooks';
import { ReviewProps } from '../Review/Review';
import { ReviewData } from '../../types/types';

interface ReviewFormProps extends ReviewProps {
}

const ReviewForm: FC<ReviewFormProps> = (props) => {
  const dispatch = useAppDispatch();
  const grades = [
    'A+', 'A', 'A-',
    'B+', 'B', 'B-',
    'C+', 'C', 'C-',
    'D+', 'D', 'D-',
    'F'
  ];

  const { loading, error, professorNames } = useProfessorNames(props.course?.id);
  const [professor, setProfessor] = useState(props.professor?.ucinetid || '');
  const [course, setCourse] = useState(props.course?.id || '');
  const [quarterTaken, setQuarterTaken] = useState('');
  const [gradeReceived, setGradeReceived] = useState('');
  const [userEmail, setUserEmail] = useState('anonymouspeter@gmail.com');
  const [userName, setUserName] = useState('Anonymous Peter');
  const [content, setContent] = useState('');
  const [quality, setQuality] = useState<number>(null!);
  const [difficulty, setDifficulty] = useState<number>(null!);
  const [submitted, setSubmitted] = useState(false);
  const [overCharLimit, setOverCharLimit] = useState(false);
  const [cookies, setCookie] = useCookies(['user']);
  const [terms, setTerms] = useState<string[]>([]);

  const fetchTerms = async () => {
    const res = await axios.get<string[]>('/schedule/getTerms?years=3');
    setTerms(res.data);
  }

  useEffect(() => {
    fetchTerms();

    // get user info from cookie
    if (cookies.hasOwnProperty('user')) {
      setUserEmail(cookies.user.email);
      setUserName(cookies.user.name);
    }
  }, [])

  const reviewRate = (e: ChangeEvent) => {
    const rating = document.getElementById(e.target.id)!.nextElementSibling;
    const name = e.target.getAttribute('name')!;
    const ratings = document.getElementsByName(name)
    for (let i = 0; i < ratings.length; i += 1) {
      ratings[i].nextElementSibling!.classList.remove('active-rating');
    }
    rating!.classList.add('active-rating');

    if (name === 'q') {
      setQuality(parseInt(e.target.id[1]));
    } else if (name === 'd') {
      setDifficulty(parseInt(e.target.id[1]));
    }
  }

  const postReview = async (review: ReviewData) => {
    const res = await axios.post<ReviewData>('/reviews', review);
    dispatch(addReview(res.data));
  }

  const submitForm = () => {
    const date = new Date();
    const year = date.getFullYear();
    const month = (1 + date.getMonth()).toString();
    const day = date.getDate().toString();
    const review = {
      professorID: professor,
      courseID: course,
      userID: userEmail,
      userDisplay: userName,
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
      setSubmitted(true);
    }
  }

  const ratings = (rating: string) => (
    <div className='form-ratings'>
      <input type='radio' name={rating} id={rating + '1'} onChange={reviewRate} />
      <label htmlFor={rating + '1'} className='r1'>1</label>
      <input type='radio' name={rating} id={rating + '2'} onChange={reviewRate} />
      <label htmlFor={rating + '2'} className='r2'>2</label>
      <input type='radio' name={rating} id={rating + '3'} onChange={reviewRate} />
      <label htmlFor={rating + '3'} className='r3'>3</label>
      <input type='radio' name={rating} id={rating + '4'} onChange={reviewRate} />
      <label htmlFor={rating + '4'} className='r4'>4</label>
      <input type='radio' name={rating} id={rating + '5'} onChange={reviewRate} />
      <label htmlFor={rating + '5'} className='r5'>5</label>
    </div>
  )

  // select instructor if in course context
  const instructorSelect = <label htmlFor='instructor'>
    <h5>Taken with:</h5>
    <select name='instructor' id='instructor' onChange={(e) => (setProfessor(document.getElementsByName(e.target.value)[0].id))}>
      <option disabled={true} selected >Instructor</option>
      {professorNames.map((instructor, i) => {
        const arr = instructor.name.split(' ');
        const name = `${arr[0][0]}. ${arr[arr.length - 1]}`
        return (
          // @ts-ignore name attribute isn't supported
          <option key={'review-form-professor-' + i} name={name} id={instructor.ucinetid}>{name}</option>
        )
      })}
    </select>
  </label>

  // select course if in professor context
  const courseSelect = <label htmlFor='course'>
    <h5>Course taken:</h5>
    <select name='course' id='course' onChange={(e) => (setCourse(document.getElementsByName(e.target.value)[0].id))}>
      <option disabled={true} selected >Course</option>
      {props.professor?.course_history.map((courseID, i) => {
        return (
          // @ts-ignore name attribute isn't supported
          <option key={'review-form-course-' + i} name={courseID} id={courseID.replace(/\s+/g, '')}>{courseID}</option>
        )
      })}
    </select>
  </label>

  const reviewForm = (
    <>
      <div className='submit-input'>
        {props.course && instructorSelect}
        {props.professor && courseSelect}
        <label htmlFor='quarter'>
          <h5>Quarter taken:</h5>
          <select name='quarter' id='quarter' onChange={(e) => setQuarterTaken(e.target.value)}>
            <option disabled={true} selected >Quarter</option>
            {terms.map((quarter, i) => (
              <option key={i}>{quarter}</option>
            ))}
          </select>
        </label>
        <label htmlFor='grade'>
          <h5>Grade:</h5>
          <select name='grade' id='grade' onChange={(e) => setGradeReceived(e.target.value)}>
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
        <div className='char-limit'>
          {overCharLimit ? (<p style={{ color: 'red' }}>Your review exceeds the character limit</p>) : null}
          <p style={content.length > 500 ? { color: 'red' } : {}} className='chars'>{content.length}/500</p>
        </div>
      </div>
      <div className='submit-rating'>
        <div>
          <h5>Quality</h5>
          {ratings('q')}
          <h5>Difficulty</h5>
          {ratings('d')}
        </div>
        <button type='button' className='rating-form-btn' onClick={submitForm}>Submit Review</button>
      </div>
    </>
  )

  return (
    <div className='submit-review'>
      {submitted ? (
        <div className='submitted-form'>
          <Icon name='check circle' size='huge' />
          <h1>Thank You</h1>
          <p>Your form has been submitted successfully.</p>
        </div>
      ) : reviewForm}
    </div>
  )
}

export default ReviewForm
