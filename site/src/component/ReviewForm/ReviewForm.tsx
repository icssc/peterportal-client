import React, { FC, useState, useEffect, useContext } from 'react';
import './ReviewForm.scss';
import { Icon } from 'semantic-ui-react';
import Form from 'react-bootstrap/Form';
import Badge from 'react-bootstrap/Badge';
import Row from 'react-bootstrap/Row';
import Col from 'react-bootstrap/Col';
import Button from 'react-bootstrap/Button';
import RangeSlider from 'react-bootstrap-range-slider';
import Modal from 'react-bootstrap/Modal';
import ReCAPTCHA from 'react-google-recaptcha';
import { addReview, editReview } from '../../store/slices/reviewSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { ReviewProps } from '../Review/Review';
import ThemeContext from '../../style/theme-context';
import { quarters } from '@peterportal/types';
import trpc from '../../trpc';
import ThankYouMessage from '../ThankYouMessage/ThankYouMessage';
import {
  anonymousName,
  EditReviewSubmission,
  grades,
  ReviewData,
  ReviewGrade,
  ReviewSubmission,
  ReviewTags,
  tags,
} from '@peterportal/types';
import spawnToast from '../../helpers/toastify';
import { useIsLoggedIn } from '../../hooks/isLoggedIn';

interface ReviewFormProps extends ReviewProps {
  closeForm: () => void;
  show: boolean;
  editing?: boolean;
  reviewToEdit?: ReviewData;
}

const ReviewForm: FC<ReviewFormProps> = ({
  closeForm,
  show,
  editing,
  reviewToEdit,
  professor: professorProp,
  course: courseProp,
}) => {
  const dispatch = useAppDispatch();
  const [professor, setProfessor] = useState(professorProp?.ucinetid ?? reviewToEdit?.professorId ?? '');
  const [course, setCourse] = useState(courseProp?.id ?? reviewToEdit?.courseId ?? '');
  const [yearTakenDefault, quarterTakenDefault] = reviewToEdit?.quarter.split(' ') ?? ['', ''];
  const [yearTaken, setYearTaken] = useState(yearTakenDefault);
  const [quarterTaken, setQuarterTaken] = useState(quarterTakenDefault);
  const [gradeReceived, setGradeReceived] = useState<ReviewGrade | undefined>(reviewToEdit?.gradeReceived);
  const [content, setContent] = useState(reviewToEdit?.content ?? '');
  const [quality, setQuality] = useState<number>(reviewToEdit?.rating ?? 3);
  const [difficulty, setDifficulty] = useState<number>(reviewToEdit?.difficulty ?? 3);
  const [takeAgain, setTakeAgain] = useState<boolean>(reviewToEdit?.takeAgain ?? false);
  const [textbook, setTextbook] = useState<boolean>(reviewToEdit?.textbook ?? false);
  const [attendance, setAttendance] = useState<boolean>(reviewToEdit?.attendance ?? false);
  const [selectedTags, setSelectedTags] = useState<ReviewTags[]>(reviewToEdit?.tags ?? []);
  const [captchaToken, setCaptchaToken] = useState('');
  const [submitted, setSubmitted] = useState(false);
  const isLoggedIn = useIsLoggedIn();
  const [anonymous, setAnonymous] = useState(reviewToEdit?.userDisplay === anonymousName);
  const [validated, setValidated] = useState(false);
  const { darkMode } = useContext(ThemeContext);
  const reviews = useAppSelector((state) => state.review.reviews);
  useEffect(() => {
    if (show) {
      // form opened
      // if not logged in, close the form
      if (!isLoggedIn) {
        spawnToast('You must be logged in to add a review!', true);
        closeForm();
      }

      setValidated(false);
      setSubmitted(false);
    }
    // we do not want closeForm to be a dependency, would cause unexpected behavior since the closeForm function is different on each render
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [show]);

  const postReview = async (review: ReviewSubmission | EditReviewSubmission) => {
    if (editing) {
      try {
        await trpc.reviews.edit.mutate(review as EditReviewSubmission);
        setSubmitted(true);
        dispatch(editReview(review as EditReviewSubmission));
      } catch (e) {
        spawnToast((e as Error).message, true);
      }
    } else {
      try {
        const res = await trpc.reviews.add.mutate(review);
        setSubmitted(true);
        dispatch(addReview(res));
      } catch (e) {
        spawnToast((e as Error).message, true);
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
    if (!valid) {
      return;
    }
    // check if CAPTCHA is completed for new reviews (captcha omitted for editing)
    if (!editing && !captchaToken) {
      spawnToast('Please complete the CAPTCHA', true);
      return;
    }
    const review = {
      id: reviewToEdit?.id,
      professorId: professor,
      courseId: course,
      anonymous: anonymous,
      content: content,
      rating: quality,
      difficulty,
      gradeReceived: gradeReceived!,
      forCredit: true,
      quarter: yearTaken + ' ' + quarterTaken,
      takeAgain,
      textbook,
      attendance,
      tags: selectedTags,
      updatedAt: new Date().toISOString(),
      captchaToken,
    };

    postReview(review);
  };

  const selectTag = (tag: ReviewTags) => {
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
        spawnToast('Cannot select more than 3 tags', true);
      }
    }
  };

  const alreadyReviewedCourseProf = (courseId: string, professorId: string) => {
    return reviews.some(
      (review) => review.courseId === courseId && review.professorId === professorId && review.authored,
    );
  };

  // select instructor if in course context
  const instructorSelect = courseProp && (
    <Form.Group>
      <Form.Label>Taken With</Form.Label>
      <Form.Control
        as="select"
        name="instructor"
        id="instructor"
        defaultValue=""
        required
        onChange={(e) => setProfessor(e.target.value)}
        value={professor}
      >
        <option disabled={true} value="">
          Instructor
        </option>
        {Object.keys(courseProp?.instructors).map((ucinetid) => {
          const name = courseProp?.instructors[ucinetid].name;
          const alreadyReviewed = alreadyReviewedCourseProf(courseProp?.id, ucinetid);
          return (
            <option
              key={ucinetid}
              value={ucinetid}
              title={alreadyReviewed ? 'You have already reviewed this professor' : undefined}
              disabled={alreadyReviewed}
            >
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
  const courseSelect = professorProp && (
    <Form.Group controlId="course">
      <Form.Label>Course Taken</Form.Label>
      <Form.Control
        as="select"
        name="course"
        id="course"
        defaultValue=""
        required
        onChange={(e) => setCourse(e.target.value)}
        value={course}
      >
        <option disabled={true} value="">
          Course
        </option>
        {Object.keys(professorProp?.courses).map((courseID) => {
          const name =
            professorProp?.courses[courseID].department + ' ' + professorProp?.courses[courseID].courseNumber;
          const alreadyReviewed = alreadyReviewedCourseProf(courseID, professorProp?.ucinetid);
          return (
            <option
              key={courseID}
              value={courseID}
              title={alreadyReviewed ? 'You have already reviewed this course' : undefined}
              disabled={alreadyReviewed}
            >
              {name}
            </option>
          );
        })}
      </Form.Control>
      <Form.Control.Feedback type="invalid">Missing course</Form.Control.Feedback>
    </Form.Group>
  );

  function editReviewHeading() {
    if (!courseProp && !professorProp) {
      return `Edit your review for ${reviewToEdit?.courseId} ${reviewToEdit?.professorId}`;
    } else if (courseProp) {
      return `Edit your review for ${courseProp?.department} ${courseProp?.courseNumber}`;
    } else {
      return `Edit your review for ${professorProp?.name}`;
    }
  }

  const reviewForm = (
    <Form noValidate validated={validated} onSubmit={submitForm}>
      <Row className="review-form-ratings">
        <Col>
          <Row>
            <Col>
              {editing ? (
                <h1>{editReviewHeading()}</h1>
              ) : (
                <h1>
                  It's your turn to review{' '}
                  {courseProp ? courseProp?.department + ' ' + courseProp?.courseNumber : professorProp?.name}
                </h1>
              )}
            </Col>
          </Row>
          <Row className="mt-4" lg={2} md={1}>
            <Col>
              <div className="review-form-section review-form-row review-form-taken">
                {instructorSelect}
                {courseSelect}
                <Form.Group>
                  <Form.Label>Grade</Form.Label>
                  <Form.Control
                    as="select"
                    name="grade"
                    id="grade"
                    defaultValue=""
                    required
                    onChange={(e) => setGradeReceived(e.target.value as ReviewGrade)}
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
                      {quarters.map((quarter) => (
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
                <Form.Label>Rate the {courseProp ? 'Course' : 'Professor'}</Form.Label>
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
                <Form.Label>Tell us more about this {courseProp ? 'course' : 'professor'}</Form.Label>
                <Form.Control
                  as="textarea"
                  placeholder="Here's your chance to be more specific..."
                  style={{ height: '15vh', width: '100%' }}
                  onChange={(e) => setContent(e.target.value)}
                  value={content}
                  maxLength={500}
                />
                <div className="char-limit">
                  <p className="chars">{content.length}/500</p>
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
                      id="anonymous"
                      label="Post as Anonymous"
                      onChange={(e: React.ChangeEvent<HTMLInputElement>) => {
                        setAnonymous(e.target.checked);
                      }}
                      checked={anonymous}
                    />
                  </Col>
                </Row>
              </Form.Group>
            </Col>
          </Row>
          <Row>
            <Col className={editing ? 'mb-3' : 'mb-3 review-form-captcha-submit'}>
              {!editing && (
                <div className="g-recaptcha">
                  <ReCAPTCHA
                    className="d-inline"
                    sitekey="6Le6rfIUAAAAAOdqD2N-QUEW9nEtfeNyzkXucLm4"
                    theme={darkMode ? 'dark' : 'light'}
                    onChange={(token) => setCaptchaToken(token ?? '')}
                  />
                </div>
              )}
              <div className="review-form-submit-cancel-buttons">
                <Button className="py-2 px-4" variant="outline-secondary" onClick={closeForm}>
                  Cancel
                </Button>
                <Button className="py-2 px-4" type="submit" variant="secondary">
                  Submit
                </Button>
              </div>
            </Col>
          </Row>
        </Col>
      </Row>
    </Form>
  );

  return (
    <Modal show={show} onHide={closeForm} centered animation={false}>
      <div className="review-form">
        {submitted ? <ThankYouMessage message="Your form has been submitted successfully." /> : reviewForm}
      </div>
    </Modal>
  );
};

export default ReviewForm;
