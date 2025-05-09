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
import { getProfessorTerms } from '../../helpers/reviews';
import { searchAPIResult, sortTerms } from '../../helpers/util';

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
  terms: termsProp,
}) => {
  const dispatch = useAppDispatch();
  const { darkMode } = useContext(ThemeContext);
  const reviews = useAppSelector((state) => state.review.reviews);
  const isLoggedIn = useIsLoggedIn();

  const [terms, setTerms] = useState<string[]>(termsProp ?? []);
  const [yearTakenDefault, quarterTakenDefault] = reviewToEdit?.quarter.split(' ') ?? ['', ''];

  const [yearTaken, setYearTaken] = useState(yearTakenDefault);

  const [quarterTaken, setQuarterTaken] = useState(quarterTakenDefault);
  const [professor, setProfessor] = useState(professorProp?.ucinetid ?? reviewToEdit?.professorId ?? '');
  const [course, setCourse] = useState(courseProp?.id ?? reviewToEdit?.courseId ?? '');
  const [gradeReceived, setGradeReceived] = useState<ReviewGrade | undefined>(reviewToEdit?.gradeReceived);
  const [difficulty, setDifficulty] = useState<number | undefined>(reviewToEdit?.difficulty);
  const [rating, setRating] = useState<number>(reviewToEdit?.rating ?? 3);
  const [takeAgain, setTakeAgain] = useState<boolean>(reviewToEdit?.takeAgain ?? false);
  const [textbook, setTextbook] = useState<boolean>(reviewToEdit?.textbook ?? false);
  const [attendance, setAttendance] = useState<boolean>(reviewToEdit?.attendance ?? false);
  const [tagsOpen, setTagsOpen] = useState(false);
  const [selectedTags, setSelectedTags] = useState<ReviewTags[]>(reviewToEdit?.tags ?? []);
  const [content, setContent] = useState(reviewToEdit?.content ?? '');
  const [captchaToken, setCaptchaToken] = useState('');
  const [anonymous, setAnonymous] = useState(reviewToEdit?.userDisplay === anonymousName);
  const [validated, setValidated] = useState(false);
  const [isSubmitting, setIsSubmitting] = useState(false);

  const [years, setYears] = useState<string[]>(termsProp ? [...new Set(terms.map((t) => t.split(' ')[0]))] : []);
  const [quarters, setQuarters] = useState<string[]>(
    termsProp ? [...new Set(terms.filter((t) => t.startsWith(yearTaken)).map((t) => t.split(' ')[1]))] : [],
  );

  useEffect(() => {
    if (!courseProp && !professorProp && reviewToEdit) {
      searchAPIResult('professor', reviewToEdit.professorId).then((professor) => {
        if (professor) {
          const profTerms = sortTerms(getProfessorTerms(professor));
          const newYears = [...new Set(profTerms.map((t) => t.split(' ')[0]))];
          const newQuarters = [
            ...new Set(profTerms.filter((t) => t.startsWith(yearTaken)).map((t) => t.split(' ')[1])),
          ];

          setTerms(profTerms);
          setYears(newYears);
          setQuarters(newQuarters);
          setYearTaken(yearTakenDefault);
          setQuarterTaken(quarterTakenDefault);
        }
      });
    }
  }, [courseProp, professorProp, reviewToEdit]);

  useEffect(() => {
    if (show) {
      // form opened
      // if not logged in, close the form
      if (!isLoggedIn) {
        spawnToast('You must be logged in to add a review!', true);
        closeForm();
      }

      setValidated(false);
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
    setIsSubmitting(true);
    try {
      if (editing) {
        await trpc.reviews.edit.mutate(review as EditReviewSubmission);
        dispatch(editReview(review as EditReviewSubmission));
        spawnToast('Your review has been edited successfully!');
      } else {
        const res = await trpc.reviews.add.mutate(review);
        dispatch(addReview(res));
        spawnToast('Your review has been submitted successfully!');
      }
      resetForm();
      closeForm();
    } catch (e) {
      spawnToast((e as Error).message, true);
    } finally {
      setIsSubmitting(false);
    }
  };

  const submitForm = (event: React.FormEvent<HTMLFormElement>) => {
    // validate form
    const form = event.currentTarget;
    const valid = form.checkValidity();
    event.preventDefault();
    event.stopPropagation();

    // prevents bootstrap form checking on valid forms
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

  // if in course context, select a professor
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

  // if in professor context, select a course
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

  function getReviewHeading() {
    if (!courseProp && !professorProp) {
      return `${reviewToEdit?.courseId}`;
    } else if (courseProp) {
      return `${courseProp?.department} ${courseProp?.courseNumber}`;
    } else {
      return `${professorProp?.name}`;
    }
  }

  const reviewForm = (
    <Modal show={show} onHide={closeForm} centered animation={false} className="ppc-modal review-form-modal">
      <Modal.Header closeButton>
        {editing ? `Edit Review for ${getReviewHeading()}` : `Review ${getReviewHeading()}`}
      </Modal.Header>
      <Modal.Body>
        {editing && (
          <p className="editing-notice">{`You are editing your review for ${reviewToEdit?.courseId} ${reviewToEdit?.professorId}.`}</p>
        )}
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
              <Form.Control.Feedback type="invalid">Missing difficulty</Form.Control.Feedback>
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
              options={tags.map((tag) => ({ label: tag, value: tag }))}
              onChange={(selected) => {
                const newTags = selected.map((opt) => opt.value);
                setSelectedTags(newTags);
                if (newTags.length > 3) {
                  setTagsOpen(false);
                }
              }}
              onMenuOpen={() => setTagsOpen(true)}
              onMenuClose={() => setTagsOpen(false)}
              menuIsOpen={selectedTags.length < 3 && tagsOpen}
              isOptionDisabled={() => selectedTags.length >= 3}
              placeholder="Select up to 3 tags"
              closeMenuOnSelect={false}
              theme={(t) => comboboxTheme(t, darkMode)}
              className="ppc-combobox"
              classNamePrefix="ppc-combobox"
            />
          </Form.Group>

          <Form.Group className="additional-details">
            <Form.Label className="ppc-modal-form-label">Additional Details (optional)</Form.Label>
            <Form.Control
              as="textarea"
              placeholder="The course was pretty good."
              onChange={(e) => setContent(e.target.value)}
              value={content}
              maxLength={500}
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

          <Button type="submit" variant="primary" disabled={isSubmitting}>
            {isSubmitting ? 'Submitting...' : 'Submit Review'}
          </Button>
        </Form>
      </Modal.Body>
    </Modal>
  );

  return reviewForm;
};

export default ReviewForm;
