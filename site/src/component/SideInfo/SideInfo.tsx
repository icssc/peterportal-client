'use client';
import { FC, useEffect, useState } from 'react';
import './SideInfo.scss';

import Badge from 'react-bootstrap/Badge';
import DropdownButton from 'react-bootstrap/DropdownButton';
import Dropdown from 'react-bootstrap/Dropdown';
import Link from 'next/link';
import { Button } from '@mui/material';

import { CourseGQLData, ProfessorGQLData, SearchType } from '../../types/types';
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { toggleFormStatus } from '../../store/slices/reviewSlice';

import RecentOfferingsTable from '../RecentOfferingsTable/RecentOfferingsTable';

interface FeaturedInfoData {
  searchType: SearchType;
  featureType: 'Highest' | 'Lowest';
  averageReviews: { [key: string]: AverageReview };
  reviewKey: string;
  displayName: string;
}

const FeaturedInfo: FC<FeaturedInfoData> = ({ searchType, featureType, averageReviews, reviewKey, displayName }) => {
  if (averageReviews[reviewKey] === undefined) {
    return null;
  }

  // rating and difficulty were constructed as totals (??)
  const { rating, difficulty, count } = averageReviews[reviewKey];

  return (
    <div className="ratings-widget">
      <div className="column">
        <p className="field-name">{featureType} Rated</p>
        <p className="field-value">
          <Link href={{ pathname: `/${searchType == 'course' ? 'professor' : 'course'}/${reviewKey}` }}>
            {displayName}
          </Link>
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
  searchType: SearchType;
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
  const allToken = 'All ' + (props.searchType == 'course' ? 'Instructors' : 'Courses');
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
      if (props.searchType == 'course') {
        key = review.professorId;
      } else if (props.searchType == 'professor') {
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
  }, [reviews, allToken, props.searchType]);

  // sort by number of reviews for the dropdown
  const sortedReviews = Object.keys(averageReviews);
  sortedReviews.sort((a, b) => averageReviews[b].count - averageReviews[a].count);

  const { count, rating, difficulty, takeAgain } = averageReviews[selectedReview] ?? {};
  const hasReviews = Object.keys(averageReviews).length > 1; // always has "All Instructors"

  return (
    <div className="side-content-wrapper">
      <div className="side-info">
        <div className="side-info-overview">
          <h2>{props.name}</h2>
          <h3>{props.title}</h3>
          <p>{props.description}</p>
          <div className="course-tags">
            {props.tags.map((tag, i) => (
              <Badge pill bg="info" key={`side-info-badge-${i}`}>
                {tag}
              </Badge>
            ))}
          </div>
        </div>

        {props.terms && <RecentOfferingsTable terms={props.terms} size="wide" />}

        <div className="side-info-ratings">
          <h2>Average Rating</h2>
          <div className="side-info-buttons">
            {/* Dropdown to select specific course/professor */}
            {/* @todo: make this select? */}
            <DropdownButton
              title={selectedReview}
              variant="secondary"
              onSelect={(e) => {
                setSelectedReview(e as string);
              }}
            >
              {sortedReviews.map((key, index) => (
                <Dropdown.Item eventKey={key} key={`side-info-dropdown-${index}`}>
                  {props.searchType == 'course' &&
                    (props.course?.instructors[key] ? props.course?.instructors[key].name : key)}
                  {props.searchType == 'professor' &&
                    (props.professor?.courses[key]
                      ? props.professor?.courses[key].department + ' ' + props.professor?.courses[key].courseNumber
                      : key)}
                </Dropdown.Item>
              ))}
            </DropdownButton>

            {/* Add a review */}
            <Button
              onClick={() => {
                dispatch(toggleFormStatus());
              }}
            >
              Rate {props.searchType}
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
                        {props.searchType.replace(/./, (x) => x.toUpperCase())} Rating
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
          {!hasReviews && (
            <span className="side-info-selected-based">No reviews found for this {props.searchType}!</span>
          )}
        </div>

        {hasReviews && (
          <div className="side-info-featured">
            <h2>{props.searchType == 'course' ? 'Instructors' : 'Courses'}</h2>
            <div className="featured-items">
              {highestReview && (
                <FeaturedInfo
                  searchType={props.searchType}
                  featureType="Highest"
                  averageReviews={averageReviews}
                  reviewKey={highestReview}
                  displayName={
                    props.searchType == 'course'
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
                  searchType={props.searchType}
                  featureType="Lowest"
                  averageReviews={averageReviews}
                  reviewKey={lowestReview}
                  displayName={
                    props.searchType == 'course'
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
