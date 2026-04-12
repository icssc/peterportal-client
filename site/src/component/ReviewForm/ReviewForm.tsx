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
import { ReviewProps } from '../Review/Review';
import React, { FC, useEffect, useState } from 'react';
import {
  Box,
  Button,
  Chip,
  Dialog,
  DialogActions,
  DialogContent,
  DialogContentText,
  DialogTitle,
  FormControl,
  FormControlLabel,
  FormLabel,
  MenuItem,
  Select,
  Slider,
  Switch,
  TextField,
} from '@mui/material';
import './ReviewForm.scss';
import { getProfessorTerms, getQuarters, getReviewHeadingName, getYears } from '../../helpers/reviews';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { searchAPIResult, sortTerms } from '../../helpers/util';
import trpc from '../../trpc';
import { addReview, editReview, setToastMsg, setToastSeverity, setShowToast } from '../../store/slices/reviewSlice';

interface ReviewFormProps extends ReviewProps {
  open: boolean;
  handleClose: () => void;
  editing?: boolean;
  reviewToEdit?: ReviewData;
}

const ReviewForm: FC<ReviewFormProps> = ({
  open,
  handleClose,
  editing,
  reviewToEdit,
  professor: professorProp,
  course: courseProp,
  terms: termsProp,
}) => {
  const dispatch = useAppDispatch();
  const reviews = useAppSelector((state) => state.review.reviews);
  const reviewHeadingName = getReviewHeadingName(reviewToEdit, courseProp, professorProp);

  const [terms, setTerms] = useState(termsProp ?? []);
  const [instructorName, setInstructorName] = useState(professorProp?.name ?? '');
  const [anonymous, setAnonymous] = useState(reviewToEdit?.userDisplay === anonymousName);
  const [yearTakenDefault, quarterTakenDefault] = reviewToEdit?.quarter.split(' ') ?? ['', ''];
  const [years, setYears] = useState(termsProp ? getYears(termsProp) : []);
  const [yearTaken, setYearTaken] = useState(yearTakenDefault);
  const [quarters, setQuarters] = useState(termsProp ? getQuarters(termsProp, yearTaken) : []);
  const [quarterTaken, setQuarterTaken] = useState(quarterTakenDefault);
  const [instructor, setInstructor] = useState(professorProp?.ucinetid ?? reviewToEdit?.professorId ?? '');
  const [course, setCourse] = useState(courseProp?.id ?? reviewToEdit?.courseId ?? '');
  const [gradeReceived, setGradeReceived] = useState<ReviewGrade | undefined>(reviewToEdit?.gradeReceived ?? undefined);

  const [selectedTags, setSelectedTags] = useState<ReviewTags[]>([]);
  const handleTagChange = (tag: ReviewTags) => {
    setSelectedTags((prev) => (prev.includes(tag) ? prev.filter((t) => t !== tag) : [...prev, tag]));
  };

  const [rating, setRating] = useState(reviewToEdit?.rating ?? 3);
  const [difficulty, setDifficulty] = useState(reviewToEdit?.difficulty ?? 3);
  const sliderMarks = [...Array(5).keys()].map((k) => ({ value: k + 1, label: '•' }));

  const [content, setContent] = useState(reviewToEdit?.content ?? '');
  const wordCount = content.match(/\S+/g)?.length ?? 0;

  const [submitting, setSubmitting] = useState(false);
  const [showFormErrors, setShowFormErrors] = useState(false);

  useEffect(() => {
    if (!professorProp && reviewToEdit) {
      searchAPIResult('instructor', reviewToEdit.professorId).then((instructor) => {
        if (instructor) {
          const instrTerms = sortTerms(getProfessorTerms(instructor));
          const newYears = [...new Set(instrTerms.map((t) => t.split(' ')[0]))];
          const newQuarters = [
            ...new Set(instrTerms.filter((t) => t.startsWith(yearTaken)).map((t) => t.split(' ')[1])),
          ];

          setTerms(instrTerms);
          setYears(newYears);
          setQuarters(newQuarters);
          setYearTaken(yearTakenDefault);
          setQuarterTaken(quarterTakenDefault);
          setInstructorName(instructor.name);
        }
      });
    }
  }, [courseProp, professorProp, quarterTakenDefault, reviewToEdit, yearTaken, yearTakenDefault]);

  useEffect(() => {
    if (yearTaken) {
      const newQuarters = getQuarters(terms, yearTaken);
      setQuarters(newQuarters);

      if (!newQuarters.includes(quarterTaken)) {
        setQuarterTaken('');
      }
    }
  }, [yearTaken, terms, quarterTaken]);

  const resetForm = () => {
    setYearTaken(yearTakenDefault);
    setQuarterTaken(quarterTakenDefault);
    setInstructor(professorProp?.ucinetid ?? reviewToEdit?.professorId ?? '');
    setCourse(courseProp?.id ?? reviewToEdit?.courseId ?? '');
    setGradeReceived(reviewToEdit?.gradeReceived);
    setDifficulty(reviewToEdit?.difficulty ?? 3);
    setRating(reviewToEdit?.rating ?? 3);
    setSelectedTags(reviewToEdit?.tags ?? []);
    setContent(reviewToEdit?.content ?? '');
    setAnonymous(reviewToEdit?.userDisplay === anonymousName);
    setShowFormErrors(false);
  };

  const postReview = async (review: ReviewSubmission | EditReviewSubmission) => {
    setSubmitting(true);
    try {
      if (editing) {
        await trpc.reviews.edit.mutate(review as EditReviewSubmission);
        dispatch(editReview(review as EditReviewSubmission));
        dispatch(setToastMsg('Your review has been edited successfully!'));
        dispatch(setToastSeverity('success'));
        dispatch(setShowToast(true));
      } else {
        const res = await trpc.reviews.add.mutate(review);
        dispatch(addReview(res));
        dispatch(setToastMsg('Your review has been submitted successfully!'));
        dispatch(setToastSeverity('success'));
        dispatch(setShowToast(true));
      }
    } catch (e) {
      dispatch(setToastMsg((e as Error).message));
      dispatch(setToastSeverity('error'));
      dispatch(setShowToast(true));
    } finally {
      setSubmitting(false);
    }

    if (!editing) resetForm();
    handleClose();
  };

  const submitForm = (event: React.FormEvent<HTMLFormElement>) => {
    const form = event.currentTarget;
    const valid = form.checkValidity();
    event.preventDefault();
    event.stopPropagation();

    if (!valid || wordCount > 500) {
      setShowFormErrors(true);
      setSubmitting(false);
      return;
    }

    setSubmitting(true);

    const review = {
      id: reviewToEdit?.id,
      professorId: instructor,
      courseId: course,
      anonymous: anonymous,
      content: content,
      rating: rating,
      difficulty: difficulty,
      gradeReceived: gradeReceived,
      forCredit: true,
      quarter: yearTaken + ' ' + quarterTaken,
      tags: selectedTags,
      updatedAt: editing ? new Date().toISOString() : undefined,
    };

    postReview(review);
  };

  const alreadyReviewedCourseInstr = (courseId: string, professorId: string) => {
    return reviews.some(
      (review) => review.courseId === courseId && review.professorId === professorId && review.authored,
    );
  };

  // if in course context, select an instructor
  const instructorSelect = courseProp && (
    <FormControl error={showFormErrors && !instructor}>
      <FormLabel required>Instructor</FormLabel>
      <Select
        name="professor"
        id="professor"
        required
        error={showFormErrors && !instructor}
        onChange={(e) => setInstructor(e.target.value)}
        value={instructor}
        displayEmpty
      >
        <MenuItem disabled value="">
          Select instructor
        </MenuItem>
        {Object.keys(courseProp?.instructors).map((ucinetid) => {
          const name = courseProp?.instructors[ucinetid].name;
          const alreadyReviewed = alreadyReviewedCourseInstr(courseProp?.id, ucinetid);
          return (
            <MenuItem
              key={ucinetid}
              value={ucinetid}
              title={alreadyReviewed ? 'You have already reviewed this instructor' : undefined}
              disabled={alreadyReviewed}
            >
              {name}
            </MenuItem>
          );
        })}
      </Select>
    </FormControl>
  );

  // if in instructor context, select a course
  const courseSelect = professorProp && (
    <FormControl error={showFormErrors && !course}>
      <FormLabel required>Course Taken</FormLabel>
      <Select
        name="course"
        id="course"
        required
        error={showFormErrors && !course}
        onChange={(e) => setCourse(e.target.value)}
        value={course}
        displayEmpty
      >
        <MenuItem disabled value="">
          Select course
        </MenuItem>
        {Object.keys(professorProp?.courses).map((courseID) => {
          const name =
            professorProp?.courses[courseID].department + ' ' + professorProp?.courses[courseID].courseNumber;
          const alreadyReviewed = alreadyReviewedCourseInstr(courseID, professorProp?.ucinetid);
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
    </FormControl>
  );

  const reviewFormContent = (
    <>
      <div className="year-quarter-row">
        <FormControl error={showFormErrors && !yearTaken}>
          <FormLabel required>Year</FormLabel>
          <Select
            name="year"
            id="year"
            required
            error={showFormErrors && !yearTaken}
            onChange={(e) => setYearTaken(e.target.value)}
            value={yearTaken}
            displayEmpty
          >
            <MenuItem disabled value="">
              Select year
            </MenuItem>
            {years?.map((term) => (
              <MenuItem key={term} value={term}>
                {term}
              </MenuItem>
            ))}
          </Select>
        </FormControl>
        <FormControl error={showFormErrors && !quarterTaken}>
          <FormLabel required>Quarter</FormLabel>
          <Select
            name="quarter"
            id="quarter"
            required
            error={showFormErrors && !quarterTaken}
            onChange={(e) => setQuarterTaken(e.target.value)}
            value={quarterTaken}
            displayEmpty
          >
            <MenuItem disabled value="">
              Select quarter
            </MenuItem>
            {quarters?.map((quarter) => (
              <MenuItem key={quarter} value={quarter}>
                {quarter}
              </MenuItem>
            ))}
          </Select>
        </FormControl>
      </div>

      <div className="course-professor-grade-row">
        {courseProp && instructorSelect}
        {professorProp && courseSelect}

        <FormControl>
          <FormLabel>Grade Received</FormLabel>
          <Select
            name="grade"
            id="grade"
            onChange={(e) => setGradeReceived(e.target.value ? (e.target.value as ReviewGrade) : undefined)}
            value={gradeReceived ?? ''}
            displayEmpty
          >
            <MenuItem value="">Prefer not to say</MenuItem>
            {grades.map((grade) => (
              <MenuItem key={grade} value={grade}>
                {grade}
              </MenuItem>
            ))}
          </Select>
        </FormControl>
      </div>

      <div className="rating-sliders">
        <FormControl fullWidth className="quality-slider">
          <div className="quality-label rating-label">
            <FormLabel>Quality Rating</FormLabel>
            <DialogContentText>Overall experience</DialogContentText>
          </div>

          <div className="labeled-slider">
            <div className="slider-value-labels">
              <span>Poor</span>
              <span>Excellent</span>
            </div>

            <Slider
              color="secondary"
              value={rating}
              onChange={(_, value) => setRating(value as number)}
              defaultValue={3}
              min={1}
              max={5}
              step={1}
              marks={sliderMarks}
            />
          </div>
        </FormControl>

        <FormControl fullWidth className="difficulty-slider">
          <div className="difficulty-label rating-label">
            <FormLabel>Difficulty Level</FormLabel>
          </div>

          <div className="labeled-slider">
            <div className="slider-value-labels">
              <span>Very Easy</span>
              <span>Very Hard</span>
            </div>
            <Slider
              color="secondary"
              value={difficulty}
              onChange={(_, value) => setDifficulty(value as number)}
              defaultValue={3}
              min={1}
              max={5}
              step={1}
              marks={sliderMarks}
            />
          </div>
        </FormControl>
      </div>

      <FormControl className="quick-tags">
        <div className="quick-tags-label">
          <FormLabel>Quick Tags</FormLabel>
          <DialogContentText>Select all that apply</DialogContentText>
        </div>
        <div className="quick-tags-select">
          {tags.map((tag) => {
            const selected = selectedTags.includes(tag);
            return (
              <Chip
                key={tag}
                label={tag}
                clickable
                color={selected ? 'secondary' : 'default'}
                variant={selected ? 'filled' : 'outlined'}
                onClick={() => handleTagChange(tag)}
              />
            );
          })}
        </div>
      </FormControl>

      <FormControl className="additional-details" error={wordCount > 500}>
        <FormLabel>Write a Review</FormLabel>
        <TextField
          multiline
          variant="outlined"
          placeholder="Share your experience — what should future students know about this course?"
          helperText={`${wordCount}/500 words`}
          onChange={(e) => setContent(e.target.value)}
          error={wordCount > 500}
          value={content}
          minRows={4}
        />
      </FormControl>
    </>
  );

  return (
    <Dialog open={open} onClose={handleClose} className="review-form-dialog">
      <DialogTitle>
        {editing ? `Edit Review for ${reviewHeadingName}` : `Review ${reviewHeadingName}`}
        <DialogContentText>{courseProp?.title}</DialogContentText>
        {editing && <DialogContentText>{`You are editing your review for ${instructorName}.`}</DialogContentText>}
      </DialogTitle>
      <Box component="form" noValidate onSubmit={submitForm}>
        <DialogContent>{reviewFormContent}</DialogContent>

        <DialogActions>
          <FormControlLabel
            className="anonymous-switch"
            value={anonymous}
            control={
              <Switch
                color="secondary"
                checked={anonymous}
                onChange={(event: React.ChangeEvent<HTMLInputElement>) => {
                  setAnonymous(event.target.checked);
                }}
              />
            }
            label="Post Anonymously"
            labelPlacement="top"
          />
          <Button variant="text" color="inherit" onClick={handleClose}>
            Cancel
          </Button>
          <Button type="submit" loading={submitting}>
            Submit Review
          </Button>
        </DialogActions>
      </Box>
    </Dialog>
  );
};

export default ReviewForm;
