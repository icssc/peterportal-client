import React, { FC, useState, useEffect, useContext } from 'react';
import './ReviewForm.scss';
import axios from 'axios';
import { useCookies } from 'react-cookie';
import { Icon } from 'semantic-ui-react';
import Form from 'react-bootstrap/Form';
import Badge from 'react-bootstrap/Badge';
import Row from 'react-bootstrap/Row';
import Col from 'react-bootstrap/Col';
import Button from 'react-bootstrap/Button';
import RangeSlider from 'react-bootstrap-range-slider';
import Modal from 'react-bootstrap/Modal';
import ReCAPTCHA from 'react-google-recaptcha';
import { addReview } from '../../store/slices/reviewSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { ReviewProps } from '../Review/Review';
import { ReviewData } from '../../types/types';
import ThemeContext from '../../style/theme-context';
import { quarterNames } from '../../helpers/planner';

interface ReviewFormProps extends ReviewProps {
  closeForm: () => void;
  editable?: boolean;
  review?: ReviewData;
}

const ReviewForm: FC<ReviewFormProps> = (props) => {
  const dispatch = useAppDispatch();
  const grades = ['A+', 'A', 'A-', 'B+', 'B', 'B-', 'C+', 'C', 'C-', 'D+', 'D', 'D-', 'F', 'P', 'NP'];
  const tags = [
    'Clear grading criteria',
    'Tough grader',
    'Amazing lectures',
    'Test heavy',
    'Get ready to read',
    'Extra credit',
    'Participation matters',
    'Graded by few things',
    "Skip class? You won't pass",
    'Accessible outside class',
    'Beware of pop quizzes',
    'Lots of homework',
    'So many papers',
    'Lecture heavy',
    'Group projects',
    'Gives good feedback',
  ];
  const [reviewId, setReviewId] = useState<string | undefined>(props.review?._id); //edit review
  const [professor, setProfessor] = useState(props.professor?.ucinetid || '');
  const [course, setCourse] = useState(props.course?.id || '');
  const [yearTaken, setYearTaken] = useState('');
  const [quarterTaken, setQuarterTaken] = useState('');
  const [gradeReceived, setGradeReceived] = useState('');
  const [userID, setUserID] = useState('');
  const [userName, setUserName] = useState('Anonymous Peter');
  const [content, setContent] = useState('');
  const [quality, setQuality] = useState<number>(3);
  const [difficulty, setDifficulty] = useState<number>(3);
  const [takeAgain, setTakeAgain] = useState<boolean>(false);
  const [textbook, setTextbook] = useState<boolean>(false);
  const [attendance, setAttendance] = useState<boolean>(false);
  const [selectedTags, setSelectedTags] = useState<string[]>([]);
  const [captchaToken, setCaptchaToken] = useState('');
  const [submitted, setSubmitted] = useState(false);
  const [overCharLimit, setOverCharLimit] = useState(false);
  const [cookies] = useCookies(['user']);
  const [validated, setValidated] = useState(false);
  const showForm = useAppSelector((state) => state.review.formOpen);
  const { darkMode } = useContext(ThemeContext);

  useEffect(() => {
    // get user info from cookie
    if (cookies.user) {
      setUserID(cookies.user.id);
      setUserName(cookies.user.name);
    }
  }, [cookies]);

  useEffect(() => {
    // upon opening this form
    if (showForm) {
      // if not logged in, close the form
      if (cookies.user === undefined) {
        alert('You must be logged in to add a review!');
        props.closeForm();
      } else {
        setSubmitted(false);
      }
    }
    //If editable is true
    if (props.review) {
      const [year, quarter] = props.review.quarter.split(' ');
      setReviewId(props.review?._id);
      setQuarterTaken(quarter);
      setYearTaken(year);
      setGradeReceived(props.review.gradeReceived);
      setDifficulty(props.review.difficulty);
      setQuality(props.review.rating);
      setContent(props.review?.reviewContent);
      setSelectedTags(props.review?.tags);
      setAttendance(props.review?.attendance);
      setTakeAgain(props.review?.takeAgain);
      setTextbook(props.review?.textbook);
      setUserName(props.review?.userDisplay);
      setProfessor(props.review?.professorID);
      setCourse(props.review?.courseID);
    }
  }, [showForm]);

  const postReview = async (review: ReviewData) => {
    if (props.editable) {
      const res = await axios.patch('/api/reviews/updateReview', review);
      if (res.data.hasOwnProperty.call(res.data, 'error')) {
        alert('You must be logged in to edit the review!');
      } else {
        setSubmitted(true);
      }
    } else {
      const res = await axios.post<ReviewData>('/api/reviews', review).catch((err) => err.response);
      if (res.status === 400) {
        alert(res.data.error ?? 'You have already submitted a review for this course/professor');
      } else if (res.data.error !== undefined) {
        alert('You must be logged in to add a review!');
      } else {
        setSubmitted(true);
        dispatch(addReview(res.data));
      }
    }
  };

  const submitForm = (event: React.FormEvent<HTMLFormElement>) => {
    // validate form
    const form = event.currentTarget;
    const valid = form.checkValidity();
    event.preventDefault();
    event.stopPropagation();

    // validated
    setValidated(true);

    // do not proceed if not valid
    if (valid === false) {
      return;
    }
    if (!captchaToken) {
      alert('Please complete the CAPTCHA');
      return;
    }
    if (props.editable === false) {
      const date = new Date();
      const year = date.getFullYear();
      const month = (1 + date.getMonth()).toString();
      const day = date.getDate().toString();
      const review = {
        professorID: professor,
        courseID: course,
        userID: userID,
        userDisplay: userName,
        reviewContent: content,
        rating: quality,
        difficulty: difficulty,
        timestamp: month + '/' + day + '/' + year,
        gradeReceived: gradeReceived,
        forCredit: true,
        quarter: yearTaken + ' ' + quarterTaken,
        score: 0,
        takeAgain: takeAgain,
        textbook: textbook,
        attendance: attendance,
        tags: selectedTags,
        captchaToken: captchaToken,
      };
      if (content.length > 500) {
        setOverCharLimit(true);
      } else {
        setOverCharLimit(false);
        postReview(review);
      }
    } else {
      const date = new Date();
      const year = date.getFullYear();
      const month = (1 + date.getMonth()).toString();
      const day = date.getDate().toString();
      const review = {
        _id: reviewId,
        professorID: professor,
        courseID: course,
        userID: userID,
        userDisplay: userName,
        reviewContent: content,
        rating: quality,
        difficulty: difficulty,
        timestamp: month + '/' + day + '/' + year,
        gradeReceived: gradeReceived,
        forCredit: true,
        quarter: yearTaken + ' ' + quarterTaken,
        score: 0,
        takeAgain: takeAgain,
        textbook: textbook,
        attendance: attendance,
        tags: selectedTags,
        verified: false,
        captchaToken: captchaToken,
      };
      if (content.length > 500) {
        setOverCharLimit(true);
      } else {
        setOverCharLimit(false);
        postReview(review);
        setSubmitted(true);
      }
    }
  };

  const selectTag = (tag: string) => {
    // remove tag
    if (selectedTags.includes(tag)) {
      const newSelectedTags = [...selectedTags];
      newSelectedTags.splice(newSelectedTags.indexOf(tag), 1);
      setSelectedTags(newSelectedTags);
    }
    // add tag if not over limit
    else {
      if (selectedTags.length < 3) {
        const newSelectedTags = [...selectedTags];
        newSelectedTags.push(tag);
        setSelectedTags(newSelectedTags);
      } else {
        alert('Cannot select more than 3 tags');
      }
    }
  };

  // select instructor if in course context
  const instructorSelect = props.course && (
    <Form.Group>
      <Form.Label>Taken With</Form.Label>
      <Form.Control
        as="select"
        name="instructor"
        id="instructor"
        defaultValue=""
        required
        onChange={(e) => setProfessor(e.target.value)}
      >
        <option disabled={true} value="">
          Instructor
        </option>
        {Object.keys(props.course?.instructors).map((ucinetid) => {
          const name = props.course?.instructors[ucinetid].shortenedName;
          return (
            <option key={ucinetid} value={ucinetid}>
              {name}
            </option>
          );
        })}
      </Form.Control>
      <Form.Text muted>
        <a href="https://forms.gle/qAhCng7Ygua7SZ358" target="_blank" rel="noopener noreferrer">
          Can't find your professor?
        </a>
      </Form.Text>
      <Form.Control.Feedback type="invalid">Missing instructor</Form.Control.Feedback>
    </Form.Group>
  );
  // select course if in professor context
  const courseSelect = props.professor && (
    <Form.Group controlId="course">
      <Form.Label>Course Taken</Form.Label>
      <Form.Control
        as="select"
        name="course"
        id="course"
        defaultValue=""
        required
        onChange={(e) => setCourse(e.target.value)}
      >
        <option disabled={true} value="">
          Course
        </option>
        {Object.keys(props.professor?.courses).map((courseID) => {
          const name =
            props.professor?.courses[courseID].department + ' ' + props.professor?.courses[courseID].courseNumber;
          return (
            <option key={courseID} value={courseID}>
              {name}
            </option>
          );
        })}
      </Form.Control>
      <Form.Control.Feedback type="invalid">Missing course</Form.Control.Feedback>
    </Form.Group>
  );

  const reviewForm = (
    <Form noValidate validated={validated} onSubmit={submitForm}>
      <Row className="review-form-ratings">
        <Col>
          <Row>
            <Col>
              {props.editable ? (
                <h1>Edit your review for {props.review?.courseID + ' ' + props.review?.professorID}</h1>
              ) : (
                <h1>
                  It's your turn to review{' '}
                  {props.course ? props.course?.department + ' ' + props.course?.courseNumber : props.professor?.name}
                </h1>
              )}
            </Col>
          </Row>
          <Row className="mt-4" lg={2} md={1}>
            <Col>
              <div className="review-form-section review-form-row review-form-taken">
                {instructorSelect}
                {courseSelect}
                <Form.Group className="review-form-grade">
                  <Form.Label>Grade</Form.Label>
                  <Form.Control
                    as="select"
                    name="grade"
                    id="grade"
                    defaultValue=""
                    required
                    onChange={(e) => setGradeReceived(e.target.value)}
                    value={gradeReceived}
                  >
                    <option disabled={true} value="">
                      Grade
                    </option>
                    {grades.map((grade) => (
                      <option key={grade}>{grade}</option>
                    ))}
                  </Form.Control>
                  <Form.Control.Feedback type="invalid">Missing grade</Form.Control.Feedback>
                </Form.Group>
              </div>
            </Col>
            <Col>
              <Form.Group className="review-form-section">
                <Form.Label>Taken During</Form.Label>
                <div className="review-form-row">
                  <Form.Group className="mr-3">
                    <Form.Control
                      as="select"
                      name="quarter"
                      id="quarter"
                      defaultValue=""
                      required
                      onChange={(e) => setQuarterTaken(e.target.value)}
                      value={quarterTaken}
                    >
                      <option disabled={true} value="">
                        Quarter
                      </option>
                      {quarterNames.map((quarter) => (
                        <option key={quarter}>{quarter}</option>
                      ))}
                    </Form.Control>
                    <Form.Control.Feedback type="invalid">Missing quarter</Form.Control.Feedback>
                  </Form.Group>
                  <Form.Group>
                    <Form.Control
                      as="select"
                      name="year"
                      id="year"
                      defaultValue=""
                      required
                      onChange={(e) => setYearTaken(e.target.value)}
                      value={yearTaken}
                    >
                      <option disabled={true} value="">
                        Year
                      </option>
                      {Array.from(new Array(10), (_, i) => new Date().getFullYear() - i).map((year) => (
                        <option key={year}>{year}</option>
                      ))}
                    </Form.Control>
                    <Form.Control.Feedback type="invalid">Missing year</Form.Control.Feedback>
                  </Form.Group>
                </div>
              </Form.Group>
            </Col>
          </Row>
          <Row className="mt-4">
            <Col>
              <Form.Group className="review-form-section">
                <Form.Label>Rate the {props.course ? 'Course' : 'Professor'}</Form.Label>
                <RangeSlider
                  min={1}
                  max={5}
                  tooltip="on"
                  value={quality}
                  onChange={(e: React.FormEvent<HTMLInputElement>) => setQuality(parseInt(e.currentTarget.value))}
                />
              </Form.Group>
            </Col>
          </Row>
          <Row className="mt-4">
            <Col>
              <Form.Group className="review-form-section">
                <Form.Label>Level of Difficulty</Form.Label>
                <RangeSlider
                  min={1}
                  max={5}
                  tooltip="on"
                  value={difficulty}
                  onChange={(e: React.FormEvent<HTMLInputElement>) => setDifficulty(parseInt(e.currentTarget.value))}
                />
              </Form.Group>
            </Col>
          </Row>
          <Row className="mt-4">
            <Col>
              <Form.Group className="review-form-section review-form-switches">
                <Row>
                  <Col>
                    <Form.Check
                      inline
                      type="switch"
                      id="takeAgain"
                      label="Would Take Again"
                      onChange={(e: React.ChangeEvent<HTMLInputElement>) => setTakeAgain(e.target.checked)}
                      checked={takeAgain}
                    />
                    <Form.Check
                      inline
                      type="switch"
                      id="textbook"
                      label="Use Textbook"
                      onChange={(e: React.ChangeEvent<HTMLInputElement>) => setTextbook(e.target.checked)}
                      checked={textbook}
                    />
                    <Form.Check
                      inline
                      type="switch"
                      id="attendance"
                      label="Mandatory Attendance"
                      onChange={(e: React.ChangeEvent<HTMLInputElement>) => setAttendance(e.target.checked)}
                      checked={attendance}
                    />
                  </Col>
                </Row>
              </Form.Group>
            </Col>
          </Row>
        </Col>
        <Col>
          <Row>
            <Col>
              <Form.Group className="review-form-section">
                <Form.Label>Select up to 3 tags</Form.Label>
                <div>
                  {tags.map((tag) => (
                    <Badge
                      key={tag}
                      pill
                      className="p-3 mr-2 mt-2"
                      variant={selectedTags.includes(tag) ? 'success' : 'info'}
                      onClick={() => {
                        selectTag(tag);
                      }}
                    >
                      {tag}
                    </Badge>
                  ))}
                </div>
              </Form.Group>
            </Col>
          </Row>
          <Row>
            <Col>
              <Form.Group className="review-form-section">
                <Form.Label>Tell us more about this {props.course ? 'course' : 'professor'}</Form.Label>
                <Form.Control
                  as="textarea"
                  placeholder="Here's your chance to be more specific..."
                  style={{ height: '15vh', width: '100%' }}
                  onChange={(e) => {
                    setContent(e.target.value);
                    if (overCharLimit && e.target.value.length < 500) {
                      setOverCharLimit(false);
                    }
                  }}
                  value={props.editable ? content : props.review?.reviewContent}
                />
                {/* <textarea rows={5} /> */}
                <div className="char-limit">
                  {overCharLimit ? <p style={{ color: 'red' }}>Your review exceeds the character limit</p> : null}
                  <p style={content.length > 500 ? { color: 'red' } : {}} className="chars">
                    {content.length}/500
                  </p>
                </div>
                <Form.Text>
                  <Icon name="warning sign" />
                  <span className="profanity-warning">
                    Refrain from using profanity, name-calling, or derogatory terms. Thank you for your contribution!
                  </span>
                </Form.Text>
              </Form.Group>
            </Col>
          </Row>
          <Row>
            <Col>
              <Form.Group className="review-form-section">
                <Row>
                  <Col>
                    <Form.Check
                      inline
                      type="switch"
                      id="anonymouse"
                      label="Post as Anonymous"
                      onChange={(e: React.ChangeEvent<HTMLInputElement>) => {
                        // set name as anonymous
                        if (e.target.checked) {
                          setUserName('Anonymous Peter');
                        }
                        // use real name
                        else {
                          setUserName(cookies.user.name);
                        }
                      }}
                      checked={userName === 'Anonymous Peter'}
                    />
                  </Col>
                </Row>
              </Form.Group>
            </Col>
          </Row>
          <Row>
            <Col className="mb-3 review-form-submit">
              <div className="g-recaptcha">
                <ReCAPTCHA
                  className="d-inline"
                  sitekey="6Le6rfIUAAAAAOdqD2N-QUEW9nEtfeNyzkXucLm4"
                  theme={darkMode ? 'dark' : 'light'}
                  onChange={(token) => setCaptchaToken(token ?? '')}
                />
              </div>
              <div>
                <Button className="py-2 px-4 float-right" type="submit" variant="secondary">
                  Submit
                </Button>
                <Button className="py-2 px-4 mr-3 float-right" variant="outline-secondary" onClick={props.closeForm}>
                  Cancel
                </Button>
              </div>
            </Col>
          </Row>
        </Col>
      </Row>
    </Form>
  );

  return (
    <Modal show={showForm} onHide={props.closeForm} centered animation={false}>
      <div className="review-form">
        {submitted ? (
          <div className="submitted-form">
            <Icon name="check circle" size="huge" />
            <h1>Thank You</h1>
            <p>Your form has been submitted successfully.</p>
          </div>
        ) : (
          reviewForm
        )}
      </div>
    </Modal>
  );
};

export default ReviewForm;
