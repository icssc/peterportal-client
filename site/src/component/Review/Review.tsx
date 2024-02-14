import { FC, useState, useEffect } from 'react';
import axios, { AxiosResponse } from 'axios';
import SubReview from './SubReview';
import ReviewForm from '../ReviewForm/ReviewForm';
import './Review.scss';

import { selectReviews, setReviews, setFormStatus } from '../../store/slices/reviewSlice';
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { CourseGQLData, ProfessorGQLData, ReviewData } from '../../types/types';
import { Checkbox, Dropdown } from 'semantic-ui-react';

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
  const [showOnlyVerifiedReviews, setShowOnlyVerifiedReviews] = useState(false);

  const getReviews = async () => {
    interface paramsProps {
      courseID?: string;
      professorID?: string;
    }
    const params: paramsProps = {};
    if (props.course) params.courseID = props.course.id;
    if (props.professor) params.professorID = props.professor.ucinetid;
    axios
      .get(`/api/reviews`, {
        params: params,
      })
      .then(async (res: AxiosResponse<ReviewData[]>) => {
        const data = res.data.filter((review) => review !== null);
        dispatch(setReviews(data));
      });
  };

  useEffect(() => {
    // prevent reviews from carrying over
    dispatch(setReviews([]));
    getReviews();
  }, [props.course?.id, props.professor?.ucinetid]);

  let sortedReviews: ReviewData[];
  // filter verified if option is set
  if (showOnlyVerifiedReviews) {
    sortedReviews = reviewData.filter((review) => review.verified);
  } else {
    // if not, clone reviewData since its const
    sortedReviews = reviewData.slice(0);
  }

  switch (sortingOption) {
    case SortingOption.MOST_RECENT:
      sortedReviews.sort((a, b) => new Date(b.timestamp).getTime() - new Date(a.timestamp).getTime());
      break;
    case SortingOption.TOP_REVIEWS: // the right side of || will fall back to most recent when score is equal
      sortedReviews.sort(
        (a, b) => b.score - a.score || new Date(b.timestamp).getTime() - new Date(a.timestamp).getTime(),
      );
      break;
    case SortingOption.CONTROVERSIAL:
      sortedReviews.sort(
        (a, b) => a.score - b.score || new Date(b.timestamp).getTime() - new Date(a.timestamp).getTime(),
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
    return (
      <>
        <div className="reviews">
          <div className="sorting-menu row">
            <Dropdown
              placeholder="Sorting Option"
              scrolling
              selection
              options={[
                { text: 'Most Recent', value: SortingOption.MOST_RECENT },
                { text: 'Top Reviews', value: SortingOption.TOP_REVIEWS },
                { text: 'Controversial', value: SortingOption.CONTROVERSIAL },
              ]}
              value={sortingOption}
              onChange={(_, s) => setSortingOption(s.value as SortingOption)}
            />
            <div id="checkbox">
              <Checkbox
                label="Show verified reviews only"
                checked={showOnlyVerifiedReviews}
                onChange={() => setShowOnlyVerifiedReviews((state) => !state)}
              />
            </div>
          </div>
          {sortedReviews.map((review) => (
            <SubReview
              review={review}
              key={review._id}
              course={props.course}
              professor={props.professor}
              userVote={review.userVote!}
            />
          ))}
          <button type="button" className="add-review-btn" onClick={openReviewForm}>
            + Add Review
          </button>
        </div>
        <ReviewForm closeForm={closeForm} {...props} />
      </>
    );
  }
};

export default Review;
