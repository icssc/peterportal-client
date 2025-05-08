import React, { FC, useState, useEffect, useContext } from 'react';
import './ReviewForm.scss';
import Form from 'react-bootstrap/Form';
import Modal from 'react-bootstrap/Modal';
import Button from 'react-bootstrap/Button';
import { addReview, editReview } from '../../store/slices/reviewSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { ReviewProps } from '../Review/Review';
import ThemeContext from '../../style/theme-context';
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
import trpc from '../../trpc';
import ReCAPTCHA from 'react-google-recaptcha';
import StarRating from './StarRating';
import Select from 'react-select';
import { comboboxTheme } from '../../helpers/courseRequirements';
import { useIsLoggedIn } from '../../hooks/isLoggedIn';

interface ReviewFormProps extends ReviewProps {
  closeForm: () => void;
  show: boolean;
  editing?: boolean;
  reviewToEdit?: ReviewData;
  // terms?: string[];
}

const ReviewForm: FC<ReviewFormProps> = ({
  closeForm,
  show,
  editing,
  reviewToEdit,
  professor: professorProp,
  course: courseProp,
  terms: termsProp,
}) => {
  const dispatch = useAppDispatch();
  const { darkMode } = useContext(ThemeContext);
  const reviews = useAppSelector((state) => state.review.reviews);
  const isLoggedIn = useIsLoggedIn();

  const [yearTakenDefault, quarterTakenDefault] = reviewToEdit?.quarter.split(' ') ?? ['', ''];
  const years = [...new Set(termsProp?.map((t) => t.split(' ')[0]))];
  const [yearTaken, setYearTaken] = useState(yearTakenDefault);
  const quarters = [...new Set(termsProp?.filter((t) => t.startsWith(yearTaken)).map((t) => t.split(' ')[1]))];
  const [quarterTaken, setQuarterTaken] = useState(quarterTakenDefault);
  const [professor, setProfessor] = useState(professorProp?.ucinetid ?? reviewToEdit?.professorId ?? '');
  const [course, setCourse] = useState(courseProp?.id ?? reviewToEdit?.courseId ?? '');
  const [gradeReceived, setGradeReceived] = useState<ReviewGrade | undefined>(reviewToEdit?.gradeReceived);
  const [difficulty, setDifficulty] = useState<number | undefined>(reviewToEdit?.difficulty);
  const [rating, setRating] = useState<number>(reviewToEdit?.rating ?? 3);
  const [takeAgain, setTakeAgain] = useState<boolean>(reviewToEdit?.takeAgain ?? false);
  const [textbook, setTextbook] = useState<boolean>(reviewToEdit?.textbook ?? false);
  const [attendance, setAttendance] = useState<boolean>(reviewToEdit?.attendance ?? false);
  const [selectedTags, setSelectedTags] = useState<ReviewTags[]>(reviewToEdit?.tags ?? []);
  const [content, setContent] = useState(reviewToEdit?.content ?? '');
  const [captchaToken, setCaptchaToken] = useState('');
  const [anonymous, setAnonymous] = useState(reviewToEdit?.userDisplay === anonymousName);
  const [validated, setValidated] = useState(false);
  // const [submitted, setSubmitted] = useState(false);

  useEffect(() => {
    if (show) {
      // form opened
      // if not logged in, close the form
      if (!isLoggedIn) {
        spawnToast('You must be logged in to add a review!', true);
        closeForm();
      }

      setValidated(false);
      // setSubmitted(false);
    }
    // we do not want closeForm to be a dependency, would cause unexpected behavior since the closeForm function is different on each render
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [show]);

  const resetForm = () => {
    setYearTaken(yearTakenDefault);
    setQuarterTaken(quarterTakenDefault);
    setProfessor(professorProp?.ucinetid ?? reviewToEdit?.professorId ?? '');
    setCourse(courseProp?.id ?? reviewToEdit?.courseId ?? '');
    setGradeReceived(reviewToEdit?.gradeReceived);
    setDifficulty(reviewToEdit?.difficulty);
    setRating(reviewToEdit?.rating ?? 3);
    setTakeAgain(reviewToEdit?.takeAgain ?? false);
    setTextbook(reviewToEdit?.textbook ?? false);
    setAttendance(reviewToEdit?.attendance ?? false);
    setSelectedTags(reviewToEdit?.tags ?? []);
    setContent(reviewToEdit?.content ?? '');
    setCaptchaToken('');
    setAnonymous(reviewToEdit?.userDisplay === anonymousName);
    setValidated(false);
  };

  const postReview = async (review: ReviewSubmission | EditReviewSubmission) => {
    if (editing) {
      try {
        await trpc.reviews.edit.mutate(review as EditReviewSubmission);
        resetForm();
        closeForm();
        spawnToast('Your review has been edited successfully!');
        dispatch(editReview(review as EditReviewSubmission));
      } catch (e) {
        spawnToast((e as Error).message, true);
      }
    } else {
      try {
        const res = await trpc.reviews.add.mutate(review);
        resetForm();
        closeForm();
        spawnToast('Your review has been submitted successfully!');
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

    // busy prop
    // Editing header

    if (!valid) {
      setValidated(true);
      return;
    }

    // for new reviews: check if CAPTCHA is completed
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
      rating: rating,
      difficulty: difficulty!,
      gradeReceived: gradeReceived!,
      forCredit: true,
      quarter: yearTaken + ' ' + quarterTaken,
      takeAgain,
      textbook,
      attendance,
      tags: selectedTags,
      updatedAt: editing ? new Date().toISOString() : undefined,
      captchaToken,
    };

    postReview(review);
  };

  const alreadyReviewedCourseProf = (courseId: string, professorId: string) => {
    return reviews.some(
      (review) => review.courseId === courseId && review.professorId === professorId && review.authored,
    );
  };

  // select professor if in course context
  const professorSelect = courseProp && (
    <Form.Group>
      <Form.Label>Professor</Form.Label>
      <Form.Control
        as="select"
        name="professor"
        id="professor"
        defaultValue=""
        required
        onChange={(e) => setProfessor(e.target.value)}
        value={professor}
        style={{ width: '100%' }}
      >
        <option disabled={true} value="">
          Select one of the following...
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
      <Form.Control.Feedback type="invalid">Missing professor</Form.Control.Feedback>
    </Form.Group>
  );

  // select course if in professor context
  const courseSelect = professorProp && (
    <Form.Group>
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
          Select one of the following...
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

  const tagOptions = tags.map((tag) => ({ label: tag, value: tag }));
  {
    /* refactor this */
  }

  const reviewForm = (
    <Modal show={show} onHide={closeForm} centered animation={false} className="ppc-modal review-form-modal">
      <Modal.Header closeButton>
        Review {courseProp ? courseProp?.department + ' ' + courseProp?.courseNumber : professorProp?.name}
      </Modal.Header>
      <Modal.Body>
        <Form noValidate validated={validated} onSubmit={submitForm} className="ppc-modal-form">
          <div className="year-quarter-row">
            <Form.Group>
              <Form.Label className="ppc-modal-form-label">Year</Form.Label>
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
                  Select
                </option>
                {years?.map((term) => (
                  <option key={term} value={term}>
                    {term}
                  </option>
                ))}
              </Form.Control>
              <Form.Control.Feedback type="invalid">Missing year</Form.Control.Feedback>
            </Form.Group>
            <Form.Group>
              <Form.Label className="ppc-modal-form-label">Quarter</Form.Label>
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
                  Select
                </option>
                {quarters?.map((term) => (
                  <option key={term} value={term}>
                    {term}
                  </option>
                ))}
              </Form.Control>
              <Form.Control.Feedback type="invalid">Missing quarter</Form.Control.Feedback>
            </Form.Group>
          </div>

          {professorSelect}
          {courseSelect}

          <div className="grade-difficulty-row">
            <Form.Group>
              <Form.Label className="ppc-modal-form-label">Grade</Form.Label>
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
                  Select
                </option>
                {grades.map((grade) => (
                  <option key={grade}>{grade}</option>
                ))}
              </Form.Control>
              <Form.Control.Feedback type="invalid">Missing grade</Form.Control.Feedback>
            </Form.Group>

            <Form.Group>
              <Form.Label className="ppc-modal-form-label">Difficulty</Form.Label>
              <Form.Control
                as="select"
                name="difficulty"
                id="difficulty"
                defaultValue=""
                required
                onChange={(e) => setDifficulty(parseInt(e.currentTarget.value))} // check this
                value={difficulty}
              >
                <option disabled={true} value="">
                  Select
                </option>
                {[1, 2, 3, 4, 5].map((difficulty) => (
                  <option key={difficulty}>{difficulty}</option>
                ))}
              </Form.Control>
            </Form.Group>
          </div>

          <Form.Group>
            <Form.Label className="ppc-modal-form-label">Rating</Form.Label>
            <StarRating rating={rating} setRating={setRating} />
          </Form.Group>

          <Form.Group>
            <Form.Label className="ppc-modal-form-label">Course Details</Form.Label>
            <Form.Check
              type="checkbox"
              id="takeAgain"
              label="Would Take Again"
              onChange={(e: React.ChangeEvent<HTMLInputElement>) => setTakeAgain(e.target.checked)}
              checked={takeAgain}
            />
            <Form.Check
              type="checkbox"
              id="textbook"
              label="Requires Textbook"
              onChange={(e: React.ChangeEvent<HTMLInputElement>) => setTextbook(e.target.checked)}
              checked={textbook}
            />
            <Form.Check
              type="checkbox"
              id="attendance"
              label="Mandatory Attendance"
              onChange={(e: React.ChangeEvent<HTMLInputElement>) => setAttendance(e.target.checked)}
              checked={attendance}
            />
          </Form.Group>

          <Form.Group>
            <Form.Label className="ppc-modal-form-label">Tags</Form.Label>
            <Select
              isMulti
              options={tagOptions}
              onChange={(selected) => {
                const newTags = selected.map((opt) => opt.value);
                setSelectedTags(newTags);
              }}
              isOptionDisabled={() => selectedTags.length >= 3}
              placeholder="Select up to 3 tags"
              closeMenuOnSelect={false}
              theme={(t) => comboboxTheme(t, darkMode)}
              className="ppc-combobox"
              classNamePrefix="ppc-combobox"
            />
          </Form.Group>

          <Form.Group>
            <Form.Label className="ppc-modal-form-label">Additional Details (optional)</Form.Label>
            <Form.Control
              as="textarea"
              placeholder="The course was pretty good."
              onChange={(e) => setContent(e.target.value)}
              value={content}
              maxLength={500}
              style={{ height: '90px' }}
            />
          </Form.Group>

          <div className="g-recaptcha">
            <ReCAPTCHA
              sitekey="6Le6rfIUAAAAAOdqD2N-QUEW9nEtfeNyzkXucLm4" //
              theme={darkMode ? 'dark' : 'light'}
              onChange={(token) => setCaptchaToken(token ?? '')}
            />
          </div>

          <Form.Group>
            <Form.Check
              type="checkbox"
              id="anonymous"
              label="Post as Anonymous"
              onChange={(e: React.ChangeEvent<HTMLInputElement>) => {
                setAnonymous(e.target.checked);
              }}
              checked={anonymous}
              className="anonymous-checkbox"
            />
          </Form.Group>

          <Button type="submit" variant="primary">
            Submit Review
          </Button>
        </Form>
      </Modal.Body>
    </Modal>
  );

  return reviewForm;
};

export default ReviewForm;
