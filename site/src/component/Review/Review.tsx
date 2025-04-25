import { FC, useState, useEffect, useCallback } from 'react';
import SubReview from './SubReview';
import ReviewForm from '../ReviewForm/ReviewForm';
import './Review.scss';

import { selectReviews, setReviews, setFormStatus } from '../../store/slices/reviewSlice';
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { CourseGQLData, ProfessorGQLData } from '../../types/types';
import { Button, Dropdown, DropdownButton, Form } from 'react-bootstrap';
import trpc from '../../trpc';
import { ReviewData } from '@peterportal/types';

export interface ReviewProps {
  course?: CourseGQLData;
  professor?: ProfessorGQLData;
}

enum SortingOption {
  MOST_RECENT,
  TOP_REVIEWS,
  CONTROVERSIAL,
}

const Review: FC<ReviewProps> = (props) => {
  const dispatch = useAppDispatch();
  const reviewData = useAppSelector(selectReviews);
  const [sortingOption, setSortingOption] = useState<SortingOption>(SortingOption.MOST_RECENT);
  const [filterOption, setFilterOption] = useState('');
  const [showOnlyVerifiedReviews, setShowOnlyVerifiedReviews] = useState(false);
  const showForm = useAppSelector((state) => state.review.formOpen);

  const getReviews = useCallback(async () => {
    interface paramsProps {
      courseId?: string;
      professorId?: string;
    }
    const params: paramsProps = {};
    if (props.course) params.courseId = props.course.id;
    if (props.professor) params.professorId = props.professor.ucinetid;
    const reviews = await trpc.reviews.get.query(params);
    dispatch(setReviews(reviews));
  }, [dispatch, props.course, props.professor]);

  useEffect(() => {
    // prevent reviews from carrying over
    dispatch(setReviews([]));
    getReviews();
  }, [dispatch, getReviews]);

  let sortedReviews: ReviewData[];
  // filter verified if option is set
  if (showOnlyVerifiedReviews) {
    sortedReviews = reviewData.filter((review) => review.verified);
  } else {
    // if not, clone reviewData since its const
    sortedReviews = reviewData.slice(0);
  }

  // calculate frequencies of professors or courses in list of reviews
  let reviewFreq = new Map<string, number>();
  if (props.course) {
    reviewFreq = sortedReviews.reduce(
      (acc, review) => acc.set(review.professorId, (acc.get(review.professorId) || 0) + 1),
      reviewFreq,
    );
  } else if (props.professor) {
    reviewFreq = sortedReviews.reduce(
      (acc, review) => acc.set(review.courseId, (acc.get(review.courseId) || 0) + 1),
      reviewFreq,
    );
  }

  if (filterOption.length > 0) {
    if (props.course) {
      // filter course reviews by specific professor
      sortedReviews = sortedReviews.filter((review) => review.professorId === filterOption);
    } else if (props.professor) {
      // filter professor reviews by specific course
      sortedReviews = sortedReviews.filter((review) => review.courseId === filterOption);
    }
  }

  switch (sortingOption) {
    case SortingOption.MOST_RECENT:
      sortedReviews.sort((a, b) => new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime());
      break;
    case SortingOption.TOP_REVIEWS: // the right side of || will fall back to most recent when score is equal
      sortedReviews.sort(
        (a, b) => b.score - a.score || new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime(),
      );
      break;
    case SortingOption.CONTROVERSIAL:
      sortedReviews.sort(
        (a, b) => a.score - b.score || new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime(),
      );
      break;
  }

  const openReviewForm = () => {
    dispatch(setFormStatus(true));
    document.body.style.overflow = 'hidden';
  };
  const closeForm = () => {
    dispatch(setFormStatus(false));
    document.body.style.overflow = 'visible';
  };

  if (!reviewData) {
    return <p>Loading reviews..</p>;
  } else {
    /** @todo refactor. last change was just pulling this out of semantic */
    const reviewSortOptions = [
      { text: 'Most Recent', value: SortingOption.MOST_RECENT },
      { text: 'Top Reviews', value: SortingOption.TOP_REVIEWS },
      { text: 'Controversial', value: SortingOption.CONTROVERSIAL },
    ];
    const selectedSortOptionText = reviewSortOptions.find((x) => x.value === sortingOption)?.text;

    const professorOptions = [{ text: 'All Professors', value: '' }].concat(
      Object.keys(props.course?.instructors ?? {})
        .map((profID) => {
          const name = `${props.course?.instructors[profID].name} (${reviewFreq.get(profID) || 0})`;
          return { text: name, value: profID };
        })
        .filter(({ value }) => reviewFreq.get(value))
        .sort((a, b) => a.text.localeCompare(b.text)),
    );
    const courseOptions = [{ text: 'All Courses', value: '' }].concat(
      Object.keys(props.professor?.courses ?? {})
        .map((courseID) => {
          const { department, courseNumber } = props.professor!.courses[courseID];
          const reviewCt = reviewFreq.get(courseID) || 0;
          const name = `${department} ${courseNumber} (${reviewCt})`;
          return { text: name, value: courseID };
        })
        .filter(({ value }) => reviewFreq.get(value))
        .sort((a, b) => a.text.localeCompare(b.text)),
    );
    const selectedProfessorOptionText = professorOptions.find((opt) => opt.value === filterOption)?.text;
    const selectedCourseOptionText = courseOptions.find((opt) => opt.value === filterOption)?.text;

    return (
      <>
        <div className="reviews">
          <div className="sort-filter-menu">
            <div className="sort-dropdown">
              <DropdownButton
                className="ppc-dropdown-btn"
                title={selectedSortOptionText}
                variant="secondary"
                onSelect={(value) => setSortingOption(parseInt(value!) as SortingOption)}
              >
                {reviewSortOptions.map((opt) => (
                  <Dropdown.Item key={opt.value} eventKey={opt.value}>
                    {opt.text}
                  </Dropdown.Item>
                ))}
              </DropdownButton>
            </div>
            {props.course && (
              <div className="filter-dropdown">
                <DropdownButton
                  className="ppc-dropdown-btn"
                  title={selectedProfessorOptionText ?? 'Select Professor...'}
                  variant="secondary"
                  onSelect={(value) => setFilterOption(value!)}
                >
                  {professorOptions.map((opt) => (
                    <Dropdown.Item key={opt.value} eventKey={opt.value}>
                      {opt.text}
                    </Dropdown.Item>
                  ))}
                </DropdownButton>
              </div>
            )}
            {props.professor && (
              <div className="filter-dropdown">
                <DropdownButton
                  className="ppc-dropdown-btn"
                  title={selectedCourseOptionText ?? 'Select Course...'}
                  variant="secondary"
                  onSelect={(value) => setFilterOption(value!)}
                >
                  {courseOptions.map((opt) => (
                    <Dropdown.Item key={opt.value} eventKey={opt.value}>
                      {opt.text}
                    </Dropdown.Item>
                  ))}
                </DropdownButton>
              </div>
            )}
            <div className="verified-only-checkbox">
              <Form>
                <Form.Check
                  type="checkbox"
                  label="Show verified reviews only"
                  id="Show verified reviews only"
                  checked={showOnlyVerifiedReviews}
                  onChange={() => setShowOnlyVerifiedReviews((state) => !state)}
                />
              </Form>
            </div>
          </div>
          {sortedReviews.length !== 0 && (
            <div className="subreviews">
              {sortedReviews.map((review) => (
                <SubReview review={review} key={review.id} course={props.course} professor={props.professor} />
              ))}
            </div>
          )}
          <Button variant="primary" className="add-review-button" onClick={openReviewForm}>
            + Add Review
          </Button>
        </div>
        <ReviewForm closeForm={closeForm} show={showForm} {...props} />
      </>
    );
  }
};

export default Review;
