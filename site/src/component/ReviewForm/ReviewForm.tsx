import React, { FC, useState, useEffect, useContext } from 'react';
import './ReviewForm.scss';
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
import Toast from '../../helpers/toast';
import { AlertColor } from '@mui/material';
import trpc from '../../trpc';
import ReCAPTCHA from 'react-google-recaptcha';
import Select2 from 'react-select';
import { comboboxTheme } from '../../helpers/courseRequirements';
import { useIsLoggedIn } from '../../hooks/isLoggedIn';
import { getProfessorTerms, getYears, getQuarters } from '../../helpers/reviews';
import { searchAPIResult, sortTerms } from '../../helpers/util';
import {
  Box,
  Checkbox,
  FormControl,
  FormControlLabel,
  FormHelperText,
  FormLabel,
  MenuItem,
  Rating,
  Select,
  TextField,
} from '@mui/material';
import { SelectChangeEvent } from '@mui/material/Select';

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
  const [professorName, setProfessorName] = useState(professorProp?.name ?? '');
  const [yearTakenDefault, quarterTakenDefault] = reviewToEdit?.quarter.split(' ') ?? ['', ''];
  const [years, setYears] = useState<string[]>(termsProp ? getYears(termsProp) : []);
  const [yearTaken, setYearTaken] = useState(yearTakenDefault);
  const [quarters, setQuarters] = useState<string[]>(termsProp ? getQuarters(termsProp, yearTaken) : []);
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
  const [showFormErrors, setShowFormErrors] = useState(false);
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [showToast, setShowToast] = useState(false);
  const [toastMsg, setToastMsg] = useState('');
  const [toastSeverity, setToastSeverity] = useState('info');

  const handleClose = () => {
    setShowToast(false);
  };

  // if no professor prop is provided when editing a review, we manually fetch the terms and names of the professor
  useEffect(() => {
    if (!professorProp && reviewToEdit) {
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
          setProfessorName(professor.name);
        }
      });
    }
  }, [courseProp, professorProp, quarterTakenDefault, reviewToEdit, yearTaken, yearTakenDefault]);

  // when a year or quarter is selected, update the valid quarters accordingly
  useEffect(() => {
    if (yearTaken) {
      const newQuarters = getQuarters(terms, yearTaken);
      setQuarters(newQuarters);

      if (!newQuarters.includes(quarterTaken)) {
        setQuarterTaken('');
      }
    }
  }, [yearTaken, terms, quarterTaken]);

  useEffect(() => {
    if (show) {
      // form opened
      // if not logged in, close the form
      if (!isLoggedIn) {
        setToastMsg('You must be logged in to add a review!');
        setToastSeverity('error');
        setShowToast(true);
        closeForm();
      }

      setShowFormErrors(false);
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
    setShowFormErrors(false);
  };

  const postReview = async (review: ReviewSubmission | EditReviewSubmission) => {
    setIsSubmitting(true);
    try {
      if (editing) {
        await trpc.reviews.edit.mutate(review as EditReviewSubmission);
        dispatch(editReview(review as EditReviewSubmission));
        setToastMsg('Your review has been edited successfully!');
        setToastSeverity('success');
        setShowToast(true);
      } else {
        const res = await trpc.reviews.add.mutate(review);
        dispatch(addReview(res));
        setToastMsg('Your review has been submitted successfully!');
        setToastSeverity('success');
        setShowToast(true);
      }
    } catch (e) {
      setToastMsg((e as Error).message);
      setToastSeverity('error');
      setShowToast(true);
    } finally {
      setIsSubmitting(false);
    }

    if (!editing) resetForm();
    closeForm();
  };

  const submitForm = (event: React.FormEvent<HTMLFormElement>) => {
    // validate form
    const form = event.currentTarget;
    const valid = form.checkValidity();
    event.preventDefault();
    event.stopPropagation();

    if (!valid) {
      setShowFormErrors(true);
      return;
    }

    // for new reviews: check if CAPTCHA is completed
    if (!editing && !captchaToken) {
      setToastMsg('Please complete the CAPTCHA');
      setToastSeverity('error');
      setShowToast(true);
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
      takeAgain: takeAgain,
      textbook: textbook,
      attendance: attendance,
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
    <FormControl error={showFormErrors && !professor}>
      <FormLabel>Professor</FormLabel>
      <Select
        name="professor"
        id="professor"
        required
        onChange={(e) => setProfessor(e.target.value)}
        value={professor}
        displayEmpty
      >
        <MenuItem disabled value="">
          Select one of the following...
        </MenuItem>
        {Object.keys(courseProp?.instructors).map((ucinetid) => {
          const name = courseProp?.instructors[ucinetid].name;
          const alreadyReviewed = alreadyReviewedCourseProf(courseProp?.id, ucinetid);
          return (
            <MenuItem
              key={ucinetid}
              value={ucinetid}
              title={alreadyReviewed ? 'You have already reviewed this professor' : undefined}
              disabled={alreadyReviewed}
            >
              {name}
            </MenuItem>
          );
        })}
      </Select>
      {showFormErrors && !professor && <FormHelperText>Missing professor</FormHelperText>}
    </FormControl>
  );

  // if in professor context, select a course
  const courseSelect = professorProp && (
    <FormControl error={showFormErrors && !course}>
      <FormLabel>Course Taken</FormLabel>
      <Select
        name="course"
        id="course"
        required
        onChange={(e) => setCourse(e.target.value)}
        value={course}
        displayEmpty
      >
        <MenuItem disabled value="">
          Select one of the following...
        </MenuItem>
        {Object.keys(professorProp?.courses).map((courseID) => {
          const name =
            professorProp?.courses[courseID].department + ' ' + professorProp?.courses[courseID].courseNumber;
          const alreadyReviewed = alreadyReviewedCourseProf(courseID, professorProp?.ucinetid);
          return (
            <MenuItem
              key={courseID}
              value={courseID}
              title={alreadyReviewed ? 'You have already reviewed this course' : undefined}
              disabled={alreadyReviewed}
            >
              {name}
            </MenuItem>
          );
        })}
      </Select>
      {showFormErrors && !course && <FormHelperText>Missing course</FormHelperText>}
    </FormControl>
  );

  function getReviewHeadingName() {
    if (!courseProp && !professorProp) {
      return `${reviewToEdit?.courseId}`;
    } else if (courseProp) {
      return `${courseProp?.department} ${courseProp?.courseNumber}`;
    } else {
      return `${professorProp?.name}`;
    }
  }

  const reviewForm = (
    <Modal show={show} onHide={closeForm} centered className="ppc-modal review-form-modal">
      <Modal.Header closeButton>
        {editing ? `Edit Review for ${getReviewHeadingName()}` : `Review ${getReviewHeadingName()}`}
      </Modal.Header>
      <Modal.Body>
        {editing && <p className="editing-notice">{`You are editing your review for ${professorName}.`}</p>}
        <Box component="form" noValidate onSubmit={submitForm}>
          <div className="year-quarter-row">
            <FormControl error={showFormErrors && !yearTaken}>
              <FormLabel>Year</FormLabel>
              <Select
                name="year"
                id="year"
                required
                onChange={(e) => setYearTaken(e.target.value)}
                value={yearTaken}
                displayEmpty
              >
                <MenuItem disabled value="">
                  Select
                </MenuItem>
                {years?.map((term) => (
                  <MenuItem key={term} value={term}>
                    {term}
                  </MenuItem>
                ))}
              </Select>
              {showFormErrors && !yearTaken && <FormHelperText>Missing year</FormHelperText>}
            </FormControl>
            <FormControl error={showFormErrors && !quarterTaken}>
              <FormLabel>Quarter</FormLabel>
              <Select
                name="quarter"
                id="quarter"
                required
                onChange={(e) => setQuarterTaken(e.target.value)}
                value={quarterTaken}
                displayEmpty
              >
                <MenuItem disabled value="">
                  Select
                </MenuItem>
                {quarters?.map((term) => (
                  <MenuItem key={term} value={term}>
                    {term}
                  </MenuItem>
                ))}
              </Select>
              {showFormErrors && !quarterTaken && <FormHelperText>Missing quarter</FormHelperText>}
            </FormControl>
          </div>

          {professorSelect}
          {courseSelect}

          <div className="grade-difficulty-row">
            <FormControl error={showFormErrors && !gradeReceived}>
              <FormLabel>Grade</FormLabel>
              <Select
                name="grade"
                id="grade"
                required
                onChange={(e) => setGradeReceived(e.target.value as ReviewGrade)}
                value={gradeReceived ?? ''}
                displayEmpty
              >
                <MenuItem disabled value="">
                  Select
                </MenuItem>
                {grades.map((grade) => (
                  <MenuItem key={grade} value={grade}>
                    {grade}
                  </MenuItem>
                ))}
              </Select>
              {showFormErrors && !gradeReceived && <FormHelperText>Missing grade</FormHelperText>}
            </FormControl>

            <FormControl error={showFormErrors && !difficulty}>
              <FormLabel>Difficulty</FormLabel>
              <Select
                name="difficulty"
                id="difficulty"
                required
                onChange={(e: SelectChangeEvent) => setDifficulty(parseInt(e.target.value))}
                value={difficulty?.toString() ?? ''}
                displayEmpty
              >
                <MenuItem disabled={true} value="">
                  Select
                </MenuItem>
                {[1, 2, 3, 4, 5].map((difficulty) => (
                  <MenuItem key={difficulty} value={difficulty}>
                    {difficulty}
                  </MenuItem>
                ))}
              </Select>
              {showFormErrors && !difficulty && <FormHelperText>Missing difficulty</FormHelperText>}
            </FormControl>
          </div>

          <FormControl error={showFormErrors && !rating}>
            <FormLabel>Rating</FormLabel>
            <Rating
              className={showFormErrors && !rating ? 'rating-error' : ''}
              size="large"
              defaultValue={3}
              onChange={(_, newValue) => {
                setRating(newValue ?? 0);
              }}
            />
            {showFormErrors && !rating && <FormHelperText>Missing rating</FormHelperText>}
          </FormControl>

          <FormControl>
            <FormLabel>Course Details</FormLabel>

            <FormControlLabel
              label="Would Take Again"
              control={<Checkbox checked={!!takeAgain} onChange={(e) => setTakeAgain(e.target.checked)} />}
            ></FormControlLabel>

            <FormControlLabel
              label="Requires Textbook"
              control={<Checkbox checked={!!textbook} onChange={(e) => setTextbook(e.target.checked)} />}
            />

            <FormControlLabel
              label="Mandatory Attendance"
              control={<Checkbox checked={!!attendance} onChange={(e) => setAttendance(e.target.checked)} />}
            />
          </FormControl>

          <FormControl>
            <FormLabel>Tags</FormLabel>
            <Select2
              isMulti
              options={tags.map((tag) => ({ label: tag, value: tag }))}
              value={selectedTags.map((tag) => ({ label: tag, value: tag }))}
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
          </FormControl>

          <FormControl className="additional-details">
            <FormLabel>Additional Details</FormLabel>
            <TextField
              multiline
              fullWidth
              placeholder="The course was pretty good."
              onChange={(e) => setContent(e.target.value)}
              value={content}
              minRows={3}
              slotProps={{
                htmlInput: {
                  maxLength: 500,
                },
              }}
            />
          </FormControl>

          {!editing && (
            <div className="g-recaptcha">
              <ReCAPTCHA
                sitekey="6Le6rfIUAAAAAOdqD2N-QUEW9nEtfeNyzkXucLm4" //
                theme={darkMode ? 'dark' : 'light'}
                onChange={(token) => setCaptchaToken(token ?? '')}
              />
            </div>
          )}

          <FormControl className="anonymous-checkbox">
            <FormControlLabel
              label="Post as Anonymous"
              control={<Checkbox checked={anonymous} onChange={(e) => setAnonymous(e.target.checked)} />}
            />
          </FormControl>

          <Button type="submit" variant="primary" disabled={isSubmitting}>
            {isSubmitting ? 'Submitting...' : 'Submit Review'}
          </Button>
        </Box>
      </Modal.Body>
      <Toast text={toastMsg} severity={toastSeverity as AlertColor} showToast={showToast} onClose={handleClose} />
    </Modal>
  );

  return reviewForm;
};

export default ReviewForm;
