import { useState, KeyboardEvent } from 'react';
import { Star, StarFill } from 'react-bootstrap-icons';
import './StarRating.scss';

const StarRating = ({ rating, setRating }: { rating: number; setRating: (r: number) => void }) => {
  const [hovered, setHovered] = useState<number>(0);

  const handleKeyDown = (e: KeyboardEvent<HTMLSpanElement>, val: number) => {
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
        >
          {val <= (hovered || rating) ? (
            <StarFill size={32} className="filled-stars" style={{ opacity: hovered ? 0.8 : 1 }} />
          ) : (
            <Star size={32} />
          )}
        </span>
      ))}
    </div>
  );
};

export default StarRating;
