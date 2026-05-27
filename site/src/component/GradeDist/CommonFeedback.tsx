import { ReviewData, ReviewTags } from '@peterportal/types';
import { FC, useCallback, useEffect, useState } from 'react';
import trpc from '../../trpc';
import { CourseGQLData, ProfessorGQLData } from '../../types/types';
import { LinearProgress } from '@mui/material';
import './CommonFeedback.scss';
import ClickableDiv from '../ClickableDiv/ClickableDiv';

interface CommonFeedbackProps {
  course?: CourseGQLData;
  professor?: ProfessorGQLData;
}

const CommonFeedback: FC<CommonFeedbackProps> = ({ course, professor }) => {
  const [reviews, setReviews] = useState<ReviewData[]>([]);
  const [showAll, setShowAll] = useState(false);

  const fetchReviews = useCallback(async () => {
    const params: { courseId?: string; professorId?: string } = {};
    if (course) params.courseId = course.id;
    if (professor) params.professorId = professor.ucinetid;
    const data = await trpc.reviews.get.query(params);
    setReviews(data);
  }, [course, professor]);

  useEffect(() => {
    setReviews([]);
    setShowAll(false);
    fetchReviews();
  }, [fetchReviews]);

  const tagCounts = new Map<ReviewTags, number>();
  reviews.forEach((review) => review.tags.forEach((tag) => tagCounts.set(tag, (tagCounts.get(tag) ?? 0) + 1)));

  const tagStats = Array.from(tagCounts.entries())
    .sort((a, b) => b[1] - a[1])
    .map(([tag, count]) => ({ label: tag, count }));

  const maxCount = tagStats[0]?.count ?? 1;

  const visibleStats = showAll ? tagStats : tagStats.slice(0, 3);

  //@todo: loading skeleton
  return (
    <div className="common-feedback">
      <div className="common-feedback-header">
        <h2>Common Feedback</h2>
        <p className="num-reviews">{reviews.length} reviews</p>
      </div>
      <div className="common-feedback-bars">
        {visibleStats.map(({ label, count }) => (
          <div key={label} className="common-feedback-bar">
            <div className="bar-label">
              <p>{label}</p>
              <p>{count}</p>
            </div>
            <LinearProgress color="primary" variant="determinate" value={(count / maxCount) * 100} />
          </div>
        ))}
      </div>
      {tagStats.length > 3 && (
        <ClickableDiv className="view-more-btn" onClick={() => setShowAll((prev) => !prev)}>
          {showAll ? 'View less' : 'View more'}
        </ClickableDiv>
      )}
    </div>
  );
};

export default CommonFeedback;
