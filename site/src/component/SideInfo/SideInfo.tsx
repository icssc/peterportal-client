import { FC, useEffect, useState } from 'react';
import './SideInfo.scss';
import Badge from 'react-bootstrap/Badge';
import DropdownButton from 'react-bootstrap/DropdownButton';
import Dropdown from 'react-bootstrap/Dropdown';
import Button from 'react-bootstrap/Button';
import { Link } from 'react-router-dom';
import CourseQuarterIndicator from '../QuarterTooltip/CourseQuarterIndicator';
import RecentOfferings from '../RecentOfferings/RecentOfferings';

import { GQLData, GQLDataType } from '../../types/types';
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { toggleFormStatus } from '../../store/slices/reviewSlice';
import { pluralize, getSentenceCase, getCourseIdFromProfessor } from '../../helpers/util';

interface AverageReview {
  count: number;
  rating: number;
  difficulty: number;
  takeAgain: number;
}

interface AverageReviews {
  [key: string]: AverageReview;
}

interface CourseSynopsisProps {
  name: string;
  title: string;
  description: string;
  tags: string[];
  terms?: string[];
}

const CourseSynopsis: FC<CourseSynopsisProps> = ({ name, title, description, tags, terms }) => {
  return (
    <div className="course-synopsis">
      <div className="title-and-offerings">
        <h2>{name}</h2>
        {terms && <CourseQuarterIndicator terms={terms} size="sm" />}
      </div>
      <h3>{title}</h3>
      <p className="description">{description}</p>
      <div className="tags">
        {tags.map((tag, i) => (
          <Badge pill variant="info" key={`side-info-badge-${i}`}>
            {tag}
          </Badge>
        ))}
      </div>
    </div>
  );
};

interface AverageRatingButtonsProps {
  data: GQLData;
  averageReviews: AverageReviews;
  selectedReview: string;
  setSelectedReview: (review: string) => void;
}

const AverageRatingButtons: FC<AverageRatingButtonsProps> = ({
  data,
  averageReviews,
  selectedReview,
  setSelectedReview,
}) => {
  const dispatch = useAppDispatch();
  const addReview = () => dispatch(toggleFormStatus());
  const sortedKeys = Object.keys(averageReviews).sort((a, b) => averageReviews[b].count - averageReviews[a].count);

  return (
    <div className="side-info-buttons">
      <DropdownButton title={selectedReview} variant="secondary" onSelect={(e) => setSelectedReview(e as string)}>
        {sortedKeys.map((key, index) => (
          <Dropdown.Item eventKey={key} key={`side-info-dropdown-${index}`}>
            {getCourseIdFromProfessor(data, key)}
          </Dropdown.Item>
        ))}
      </DropdownButton>
      <Button variant="primary" onClick={addReview}>
        Rate {data.type}
      </Button>
    </div>
  );
};

interface AverageRatingStatsProps {
  dataType: GQLDataType;
  averageReviews: AverageReviews;
  selectedReview: string;
}

const AverageRatingStats: FC<AverageRatingStatsProps> = ({ dataType, averageReviews, selectedReview }) => {
  if (Object.keys(averageReviews).length <= 1) {
    return <span className="side-info-selected-based">No reviews found for this {dataType}!</span>;
  }

  const { count, rating, difficulty, takeAgain } = averageReviews[selectedReview];
  const reviewData = [
    { label: `${getSentenceCase(dataType)} Rating`, value: (rating / count).toFixed(2) },
    { label: 'Would Take Again', value: ((100 * takeAgain) / count).toFixed(0) + '%' },
    { label: 'Difficulty Level', value: (difficulty / count).toFixed(2) },
  ];

  return (
    <>
      <div className="side-info-selected-based">
        Based on {count} review{pluralize(count)}
      </div>
      <div className="side-info-selected-rating">
        {reviewData.map(({ label, value }) => (
          <div className="side-info-stat" key={label}>
            <div className="side-info-stat-label">{label}</div>
            <div className="side-info-stat-value">{value}</div>
          </div>
        ))}
      </div>
    </>
  );
};

interface FeaturedItemData {
  data: GQLData;
  averageReviews: AverageReviews;
  featureType: 'Highest' | 'Lowest';
  reviewKey: string;
}

const FeaturedItem: FC<FeaturedItemData> = ({ data, averageReviews, featureType, reviewKey }) => {
  const linkPath = `/${data.type === 'course' ? 'professor' : 'course'}/${reviewKey}`;
  const { rating, difficulty, count } = averageReviews[reviewKey];
  const courseId = getCourseIdFromProfessor(data, reviewKey);

  const columns = [
    { label: `${featureType} Rated`, value: <Link to={{ pathname: linkPath }}>{courseId}</Link> },
    { label: 'Rating', value: `${(rating / count).toFixed(2)} / 5` },
    { label: 'Difficulty', value: `${(difficulty / count).toFixed(2)} / 5` },
  ];

  return (
    <div className="ratings-widget">
      {columns.map(({ label, value }, index) => (
        <div className="column" key={index}>
          <p className="field-name">{label}</p>
          <p className="field-value">{value}</p>
        </div>
      ))}
    </div>
  );
};

interface SideInfoProps {
  data: GQLData;
  name: string;
  title: string;
  description: string;
  tags: string[];
  terms?: string[];
}

const SideInfo: FC<SideInfoProps> = ({ data, name, title, description, tags, terms }) => {
  const reviews = useAppSelector((state) => state.review.reviews);
  const [averageReviews, setAverageReviews] = useState<AverageReviews>({});
  const [selectedReview, setSelectedReview] = useState('');
  const [highestReview, setHighestReview] = useState('');
  const [lowestReview, setLowestReview] = useState('');
  const capitalizedOtherDataType = data.type === 'course' ? 'Instructors' : 'Courses';

  useEffect(() => {
    const newAverageReviews: AverageReviews = {};
    const allReviews: AverageReview = {
      count: 0,
      rating: 0,
      difficulty: 0,
      takeAgain: 0,
    };

    reviews.forEach((review) => {
      // determine key based on data type
      const key = data.type === 'course' ? review.professorId : review.courseId;

      // add review entry
      if (!Object.prototype.hasOwnProperty.call(newAverageReviews, key)) {
        newAverageReviews[key] = {
          count: 0,
          rating: 0,
          difficulty: 0,
          takeAgain: 0,
        };
      }

      // update review entry
      newAverageReviews[key].count += 1;
      newAverageReviews[key].rating += review.rating;
      newAverageReviews[key].difficulty += review.difficulty;
      newAverageReviews[key].takeAgain += review.takeAgain ? 1 : 0;

      // update all reviews entry
      allReviews.count += 1;
      allReviews.rating += review.rating;
      allReviews.difficulty += review.difficulty;
      allReviews.takeAgain += review.takeAgain ? 1 : 0;
    });

    // find highest and lowest reviews
    const sortedKeys = Object.keys(newAverageReviews).sort(
      (a, b) =>
        newAverageReviews[a].rating / newAverageReviews[a].count -
        newAverageReviews[b].rating / newAverageReviews[b].count,
    );

    // set the all token to all reviews
    const allToken = `All ${capitalizedOtherDataType}`;
    newAverageReviews[allToken] = allReviews;

    // set reviews to state
    setAverageReviews(newAverageReviews);
    setSelectedReview(allToken);
    if (sortedKeys.length > 0) {
      setHighestReview(sortedKeys[sortedKeys.length - 1]);
      setLowestReview(sortedKeys[0]);
    }
  }, [reviews, data, capitalizedOtherDataType]);

  return (
    <div className="side-info">
      <CourseSynopsis name={name} title={title} description={description} tags={tags} terms={terms} />

      {terms?.length && <RecentOfferings terms={terms} />}

      <div className="side-info-ratings">
        <h2>Average Rating</h2>
        <AverageRatingButtons
          data={data}
          averageReviews={averageReviews}
          selectedReview={selectedReview}
          setSelectedReview={setSelectedReview}
        />
        <AverageRatingStats dataType={data.type} averageReviews={averageReviews} selectedReview={selectedReview} />
      </div>

      {Object.keys(averageReviews).length > 1 && (
        <div className="side-info-featured">
          <h2>{capitalizedOtherDataType}</h2>
          <div className="featured-items">
            <FeaturedItem data={data} averageReviews={averageReviews} featureType="Highest" reviewKey={highestReview} />
            <FeaturedItem data={data} averageReviews={averageReviews} featureType="Lowest" reviewKey={lowestReview} />
          </div>
        </div>
      )}
    </div>
  );
};

export default SideInfo;
