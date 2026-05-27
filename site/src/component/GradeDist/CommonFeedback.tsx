import { ReviewData, ReviewTags } from '@peterportal/types';
import { FC, useCallback, useEffect, useState } from 'react';
import trpc from '../../trpc';
import { CourseGQLData, ProfessorGQLData } from '../../types/types';
import { LinearProgress } from '@mui/material';
import './CommonFeedback.scss';

interface CommonFeedbackProps {
  course?: CourseGQLData;
  professor?: ProfessorGQLData;
}

const CommonFeedback: FC<CommonFeedbackProps> = ({ course, professor }) => {
  const [reviews, setReviews] = useState<ReviewData[]>([]);

  const fetchReviews = useCallback(async () => {
    const params: { courseId?: string; professorId?: string } = {};
    if (course) params.courseId = course.id;
    if (professor) params.professorId = professor.ucinetid;
    const data = await trpc.reviews.get.query(params);
    setReviews(data);
  }, [course, professor]);

  useEffect(() => {
    setReviews([]);
    fetchReviews();
  }, [fetchReviews]);

  const tagCounts = new Map<ReviewTags, number>();
  reviews.forEach((review) => review.tags.forEach((tag) => tagCounts.set(tag, (tagCounts.get(tag) ?? 0) + 1)));

  const tagStats = Array.from(tagCounts.entries())
    .sort((a, b) => b[1] - a[1])
    .map(([tag, count]) => ({ label: tag, count }));

  const maxCount = tagStats[0]?.count ?? 1;

  return (
    <div className="common-feedback">
      <div className="common-feedback-header">
        <h2>Common Feedback</h2>
        <p className="num-reviews">{reviews.length} reviews</p>
      </div>
      <div className="common-feedback-bars">
        {tagStats.map(({ label, count }) => (
          <div key={label} className="common-feedback-bar">
            <div className="bar-label">
              <p>{label}</p>
              <p>{count}</p>
            </div>
            <LinearProgress variant="determinate" value={(count / maxCount) * 100} />
          </div>
        ))}
      </div>
    </div>
  );
};

export default CommonFeedback;
