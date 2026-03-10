import { anonymousName, grades, ReviewData, ReviewGrade } from '@peterportal/types';
import { ReviewProps } from '../Review/Review';
import React, { FC, useEffect, useState } from 'react';
import {
  Box,
  Button,
  Card,
  CardContent,
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
import { getProfessorTerms, getQuarters, getYears } from '../../helpers/reviews';
import { useAppSelector } from '../../store/hooks';
import { searchAPIResult, sortTerms } from '../../helpers/util';

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
  const reviews = useAppSelector((state) => state.review.reviews);

  const [terms, setTerms] = useState<string[]>(termsProp ?? []);
  const [professorName, setProfessorName] = useState(professorProp?.name ?? '');
  const [submitting, setSubmitting] = useState(false);
  const [anonymous, setAnonymous] = useState(reviewToEdit?.userDisplay === anonymousName);
  const [yearTakenDefault, quarterTakenDefault] = reviewToEdit?.quarter.split(' ') ?? ['', ''];
  const [years, setYears] = useState<string[]>(termsProp ? getYears(termsProp) : []);
  const [yearTaken, setYearTaken] = useState(yearTakenDefault);
  const [quarters, setQuarters] = useState<string[]>(termsProp ? getQuarters(termsProp, yearTaken) : []);
  const [quarterTaken, setQuarterTaken] = useState(quarterTakenDefault);
  const [professor, setProfessor] = useState(professorProp?.ucinetid ?? reviewToEdit?.professorId ?? '');
  const [course, setCourse] = useState(courseProp?.id ?? reviewToEdit?.courseId ?? '');
  const [gradeReceived, setGradeReceived] = useState<ReviewGrade | undefined>(reviewToEdit?.gradeReceived);

  const [quality, setQuality] = useState<number>(reviewToEdit?.rating ?? 3);
  const [difficulty, setDifficulty] = useState<number | undefined>(reviewToEdit?.difficulty);

  const [content, setContent] = useState(reviewToEdit?.content ?? '');

  useEffect(() => {
    if (!professorProp && reviewToEdit) {
      searchAPIResult('instructor', reviewToEdit.professorId).then((professor) => {
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

  useEffect(() => {
    if (yearTaken) {
      const newQuarters = getQuarters(terms, yearTaken);
      setQuarters(newQuarters);

      if (!newQuarters.includes(quarterTaken)) {
        setQuarterTaken('');
      }
    }
  }, [yearTaken, terms, quarterTaken]);

  function getReviewHeadingName() {
    if (!courseProp && !professorProp) {
      return `${reviewToEdit?.courseId}`;
    } else if (courseProp) {
      return `${courseProp?.department} ${courseProp?.courseNumber}`;
    } else {
      return `${professorProp?.name}`;
    }
  }

  const submitForm = (event: React.FormEvent<HTMLFormElement>) => {
    // validate form
    setSubmitting(true);

    // const form = event.currentTarget;
    // const valid = form.checkValidity();
    event.preventDefault();
    event.stopPropagation();

    // if (!valid) {
    //   setShowFormErrors(true);
    //   return;
    // }

    // const review = {
    //   id: reviewToEdit?.id,
    //   professorId: professor,
    //   courseId: course,
    //   anonymous: anonymous,
    //   content: content,
    //   rating: rating,
    //   difficulty: difficulty!,
    //   gradeReceived: gradeReceived!,
    //   forCredit: true,
    //   quarter: yearTaken + ' ' + quarterTaken,
    //   takeAgain: takeAgain,
    //   textbook: textbook,
    //   attendance: attendance,
    //   tags: selectedTags,
    //   updatedAt: editing ? new Date().toISOString() : undefined,
    // };

    // postReview(review);
  };

  const alreadyReviewedCourseProf = (courseId: string, professorId: string) => {
    return reviews.some(
      (review) => review.courseId === courseId && review.professorId === professorId && review.authored,
    );
  };

  // if in course context, select a professor
  const professorSelect = courseProp && (
    <FormControl>
      <FormLabel required>Instructor</FormLabel>
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

  // if in professor context, select a course
  const courseSelect = professorProp && (
    <FormControl>
      <FormLabel required>Course Taken</FormLabel>
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
    </FormControl>
  );

  const reviewFormContent = (
    <Box component="form" noValidate onSubmit={submitForm}>
      <div className="year-quarter-row">
        <FormControl>
          <FormLabel required>Year</FormLabel>
          <Select
            name="year"
            id="year"
            required
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
        <FormControl>
          <FormLabel required>Quarter</FormLabel>
          <Select
            name="quarter"
            id="quarter"
            required
            onChange={(e) => setQuarterTaken(e.target.value)}
            value={quarterTaken}
            displayEmpty
          >
            <MenuItem disabled value="">
              Select quarter
            </MenuItem>
            {quarters?.map((term) => (
              <MenuItem key={term} value={term}>
                {term}
              </MenuItem>
            ))}
          </Select>
        </FormControl>
      </div>

      <div className="course-professor-grade-row">
        {courseProp && professorSelect}
        {professorProp && courseSelect}

        <FormControl>
          {' '}
          {/* @todo: Prefer not to say */}
          <FormLabel>Grade Received</FormLabel>
          <Select
            name="grade"
            id="grade"
            required
            onChange={(e) => setGradeReceived(e.target.value as ReviewGrade)}
            value={gradeReceived ?? ''}
            displayEmpty
          >
            <MenuItem disabled value="">
              Prefer not to say
            </MenuItem>
            {grades.map((grade) => (
              <MenuItem key={grade} value={grade}>
                {grade}
              </MenuItem>
            ))}
          </Select>
        </FormControl>
      </div>

      <Card variant="outlined">
        <CardContent>
          <FormControl fullWidth>
            <FormLabel>Quality Rating</FormLabel>
            <Slider
              color="secondary"
              value={quality}
              onChange={(_, value) => setQuality(value as number)}
              defaultValue={3}
              min={1}
              max={5}
              step={1}
              marks={[
                { value: 1, label: '1' },
                { value: 2, label: '2' },
                { value: 3, label: '3' },
                { value: 4, label: '4' },
                { value: 5, label: '5' },
              ]}
              valueLabelDisplay="auto"
            />
          </FormControl>

          <FormControl fullWidth>
            <FormLabel>Difficulty Level</FormLabel>
            <Slider
              color="secondary"
              value={difficulty}
              onChange={(_, value) => setDifficulty(value as number)}
              defaultValue={3}
              min={1}
              max={5}
              step={1}
              marks={[
                { value: 1, label: '1' },
                { value: 2, label: '2' },
                { value: 3, label: '3' },
                { value: 4, label: '4' },
                { value: 5, label: '5' },
              ]}
              valueLabelDisplay="auto"
            />
          </FormControl>
        </CardContent>
      </Card>

      <FormControl>
        {' '}
        {/* @todo: resizable */}
        <FormLabel>Write a Review</FormLabel>
        <TextField
          multiline
          variant="outlined"
          placeholder="Share your experience — what should future students know about this course? "
          onChange={(e) => setContent(e.target.value)}
          value={content}
          minRows={2}
          slotProps={{
            htmlInput: {
              maxLength: 500,
            },
          }}
        />
      </FormControl>
    </Box>
  );

  return (
    <Dialog open={open} onClose={handleClose} className="review-form-dialog">
      <DialogTitle>
        {editing ? `Edit Review for ${getReviewHeadingName()}` : `Review ${getReviewHeadingName()}`}
        <DialogContentText>{courseProp?.title}</DialogContentText> {/* if professor, put something?*/}
        {editing && <DialogContentText>{`You are editing your review for ${professorName}.`}</DialogContentText>}
      </DialogTitle>

      <DialogContent>{reviewFormContent}</DialogContent>

      <DialogActions>
        <FormControlLabel
          value={anonymous}
          control={
            <Switch
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
    </Dialog>
  );
};

export default ReviewForm;
