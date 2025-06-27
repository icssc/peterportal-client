import { FC } from 'react';
import { useState } from 'react';
import './StarRating.scss';

import StarIcon from '@mui/icons-material/Star';
import StarBorderIcon from '@mui/icons-material/StarBorder';

interface StarRatingProps {
  rating: number;
  setRating: (rating: number) => void;
}

const StarRating: FC<StarRatingProps> = ({ rating, setRating }) => {
  const [hovered, setHovered] = useState<number>(0);

  const handleKeyDown = (e: React.KeyboardEvent<HTMLSpanElement>, val: number) => {
    if (e.key === 'Enter' || e.key === ' ') {
      e.preventDefault();
      setRating(val);
    }
  };

  return (
    <div
      className="review-star-rating"
      onMouseLeave={() => setHovered(0)}
      role="radiogroup"
      aria-label="Star rating"
      tabIndex={0}
    >
      {[1, 2, 3, 4, 5].map((val) => (
        <span
          key={val}
          onClick={() => setRating(val)}
          onMouseEnter={() => setHovered(val)}
          onKeyDown={(e) => handleKeyDown(e, val)}
          tabIndex={0}
          role="radio"
          aria-checked={rating === val}
          aria-label={`${val} star${val !== 1 ? 's' : ''}`}
          title={(hovered === rating ? 'Your current rating is' : 'Change rating to') + ` ${val} stars`}
        >
          {val <= (hovered || rating) ? (
            <StarIcon className={`filled-star ${hovered && hovered !== rating ? 'tentative' : ''}`} />
          ) : (
            <StarBorderIcon />
          )}
        </span>
      ))}
    </div>
  );
};

export default StarRating;
