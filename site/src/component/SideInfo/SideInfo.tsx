import { FC, useEffect, useState } from 'react';
import './SideInfo.scss';
import Badge from 'react-bootstrap/Badge';
import DropdownButton from 'react-bootstrap/DropdownButton';
import Dropdown from 'react-bootstrap/Dropdown';
import Button from 'react-bootstrap/Button';
import { Link } from 'react-router-dom';
import CourseQuarterIndicator from '../QuarterTooltip/CourseQuarterIndicator';

import { CourseGQLData, ProfessorGQLData, DataType } from '../../types/types';
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { toggleFormStatus } from '../../store/slices/reviewSlice';
import RecentOfferings from '../RecentOfferings/RecentOfferings';

interface FeaturedInfoData {
  dataType: DataType;
  featureType: 'Highest' | 'Lowest';
  averageReviews: { [key: string]: AverageReview };
  reviewKey: string;
  displayName: string;
}

const FeaturedInfo: FC<FeaturedInfoData> = ({ dataType, featureType, averageReviews, reviewKey, displayName }) => {
  if (!averageReviews[reviewKey]) return null;

  // rating and difficulty were constructed as totals (??)
  const { rating, difficulty, count } = averageReviews[reviewKey];

  return (
    <div className="ratings-widget">
      <div className="column">
        <p className="field-name">{featureType} Rated</p>
        <p className="field-value">
          <Link to={{ pathname: `/${dataType == 'course' ? 'professor' : 'course'}/${reviewKey}` }}>{displayName}</Link>
        </p>
      </div>
      <div className="column">
        <p className="field-name">Rating</p>
        <p className="field-value">{(rating / count).toFixed(2)} / 5</p>
      </div>
      <div className="column">
        <p className="field-name">Difficulty</p>
        <p className="field-value">{(difficulty / count).toFixed(2)} / 5</p>
      </div>
    </div>
  );
};

interface SideInfoProps {
  dataType: DataType;
  name: string;
  title: string;
  description: string;
  tags: string[];
  course?: CourseGQLData;
  professor?: ProfessorGQLData;
  terms?: string[];
}

interface AverageReview {
  count: number;
  rating: number;
  difficulty: number;
  takeAgain: number;
}

const SideInfo: FC<SideInfoProps> = (props) => {
  const dispatch = useAppDispatch();
  const allToken = 'All ' + (props.dataType == 'course' ? 'Instructors' : 'Courses');
  const reviews = useAppSelector((state) => state.review.reviews);
  const [averageReviews, setAverageReviews] = useState<{ [key: string]: AverageReview }>({});
  const [highestReview, setHighestReview] = useState('');
  const [lowestReview, setLowestReview] = useState('');
  const [selectedReview, setSelectedReview] = useState('');

  useEffect(() => {
    const newAverageReviews: { [key: string]: AverageReview } = {};
    const allReviews = {
      count: 0,
      rating: 0,
      difficulty: 0,
      takeAgain: 0,
    };

    reviews.forEach((review) => {
      let key = '';
      // determine the key to group reviews by
      if (props.dataType == 'course') {
        key = review.professorId;
      } else if (props.dataType == 'professor') {
        key = review.courseId;
      }

      // add review entry
      if (!Object.prototype.hasOwnProperty.call(newAverageReviews, key)) {
        newAverageReviews[key] = {
          count: 0,
          rating: 0,
          difficulty: 0,
          takeAgain: 0,
        };
      }
      newAverageReviews[key].count += 1;
      newAverageReviews[key].rating += review.rating;
      newAverageReviews[key].difficulty += review.difficulty;
      newAverageReviews[key].takeAgain += review.takeAgain ? 1 : 0;
      allReviews.count += 1;
      allReviews.rating += review.rating;
      allReviews.difficulty += review.difficulty;
      allReviews.takeAgain += review.takeAgain ? 1 : 0;
    });

    // find highest and lowest reviews
    const sortedKeys = Object.keys(newAverageReviews);
    sortedKeys.sort(
      (a, b) =>
        newAverageReviews[a].rating / newAverageReviews[a].count -
        newAverageReviews[b].rating / newAverageReviews[b].count,
    );

    // set the all token to all reviews
    newAverageReviews[allToken] = allReviews;

    // set reviews to state
    setAverageReviews(newAverageReviews);
    setSelectedReview(allToken);
    if (sortedKeys.length > 0) {
      setHighestReview(sortedKeys[sortedKeys.length - 1]);
      setLowestReview(sortedKeys[0]);
    }
  }, [reviews, allToken, props.dataType]);

  // sort by number of reviews for the dropdown
  const sortedReviews = Object.keys(averageReviews);
  sortedReviews.sort((a, b) => averageReviews[b].count - averageReviews[a].count);

  const { count, rating, difficulty, takeAgain } = averageReviews[selectedReview] ?? {};
  const hasReviews = Object.keys(averageReviews).length > 1; // always has "All Instructors"

  return (
    <div className="side-content-wrapper">
      <div className="side-info">
        <div className="course-synopsis">
          <div className="title-and-offerings">
            <h2>{props.name}</h2>
            {props.terms && <CourseQuarterIndicator terms={props.terms} size="sm" />}
          </div>
          <h3>{props.title}</h3>

          <p className="description">{props.description}</p>
          <div className="tags">
            {props.tags.map((tag, i) => (
              <Badge pill variant="info" key={`side-info-badge-${i}`}>
                {tag}
              </Badge>
            ))}
          </div>
        </div>

        {props.terms?.length ? <RecentOfferings terms={props.terms} /> : null}

        <div className="side-info-ratings">
          <h2>Average Rating</h2>
          <div className="side-info-buttons">
            {/* Dropdown to select specific course/professor */}
            <DropdownButton
              title={selectedReview}
              variant="secondary"
              onSelect={(e) => {
                setSelectedReview(e as string);
              }}
            >
              {sortedReviews.map((key, index) => (
                <Dropdown.Item eventKey={key} key={`side-info-dropdown-${index}`}>
                  {props.dataType == 'course' &&
                    (props.course?.instructors[key] ? props.course?.instructors[key].name : key)}
                  {props.dataType == 'professor' &&
                    (props.professor?.courses[key]
                      ? props.professor?.courses[key].department + ' ' + props.professor?.courses[key].courseNumber
                      : key)}
                </Dropdown.Item>
              ))}
            </DropdownButton>

            {/* Add a review */}
            <Button
              variant="primary"
              onClick={() => {
                dispatch(toggleFormStatus());
              }}
            >
              Rate {props.dataType}
            </Button>
          </div>
          {hasReviews && (
            <>
              {/* Show stats of selected course/professor */}
              {selectedReview && (
                <>
                  <div className="side-info-selected-based">Based on {count} reviews</div>
                  <div className="side-info-selected-rating">
                    <div className="side-info-stat">
                      <div className="side-info-stat-label">
                        {props.dataType.replace(/./, (x) => x.toUpperCase())} Rating
                      </div>
                      <div className="side-info-stat-value">{count > 0 ? (rating / count).toFixed(2) : '\u2013'}</div>
                    </div>

                    <div className="side-info-stat">
                      <div className="side-info-stat-label">Would Take Again</div>
                      <div className="side-info-stat-value">
                        {count > 0 ? ((takeAgain / count) * 100).toFixed(0) + '%' : '\u2013'}
                      </div>
                    </div>

                    <div className="side-info-stat">
                      <div className="side-info-stat-label">Difficulty Level</div>
                      <div className="side-info-stat-value">
                        {count > 0 ? (difficulty / count).toFixed(2) : '\u2013'}
                      </div>
                    </div>
                  </div>
                </>
              )}
            </>
          )}
          {!hasReviews && <span className="side-info-selected-based">No reviews found for this {props.dataType}!</span>}
        </div>

        {hasReviews && (
          <div className="side-info-featured">
            <h2>{props.dataType == 'course' ? 'Instructors' : 'Courses'}</h2>
            <div className="featured-items">
              {highestReview && (
                <FeaturedInfo
                  dataType={props.dataType}
                  featureType="Highest"
                  averageReviews={averageReviews}
                  reviewKey={highestReview}
                  displayName={
                    props.dataType == 'course'
                      ? (Object.values(props.course?.instructors ?? {})?.find(
                          ({ ucinetid }) => ucinetid === highestReview,
                        )?.name ?? '')
                      : props.professor?.courses[highestReview]
                        ? props.professor?.courses[highestReview].department +
                          ' ' +
                          props.professor?.courses[highestReview].courseNumber
                        : highestReview
                  }
                />
              )}
              {lowestReview && (
                <FeaturedInfo
                  dataType={props.dataType}
                  featureType="Lowest"
                  averageReviews={averageReviews}
                  reviewKey={lowestReview}
                  displayName={
                    props.dataType == 'course'
                      ? (Object.values(props.course?.instructors ?? {})?.find(
                          ({ ucinetid }) => ucinetid === lowestReview,
                        )?.name ?? '')
                      : props.professor?.courses[lowestReview]
                        ? props.professor?.courses[lowestReview].department +
                          ' ' +
                          props.professor?.courses[lowestReview].courseNumber
                        : lowestReview
                  }
                />
              )}
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

export default SideInfo;
