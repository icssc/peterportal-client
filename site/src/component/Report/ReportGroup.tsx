import { FC, useEffect, useState } from 'react';
import Button from 'react-bootstrap/Button';
import './ReportGroup.scss';
import SubReport from './SubReport';
import trpc from '../../trpc';
import { ReportData, ReviewData } from '@peterportal/types';

interface ReportGroupProps {
  reviewId: number;
  reports: ReportData[];
  onAccept: () => void;
  onDeny: () => void;
}

const ReportGroup: FC<ReportGroupProps> = (props) => {
  const [review, setReview] = useState<ReviewData>(null!);

  const getReviewData = async (reviewId: number) => {
    const review = (await trpc.reviews.get.query({ reviewId }))[0];
    setReview(review);
  };

  useEffect(() => {
    getReviewData(props.reviewId);
  }, [props.reviewId]);

  if (!review) return null;

  return (
    <div className="report-group">
      <div className="report-group-identifier">
        <div className="report-group-professor-name">{review.professorId}</div>
        <div className="report-group-course-id">{review.courseId}</div>
        <div className="report-group-user-display">
          Posted by {review.userDisplay} on{' '}
          {new Date(review.createdAt).toLocaleString('default', { year: 'numeric', month: 'long', day: 'numeric' })}
        </div>
      </div>
      <div className="report-group-content">
        <p className="report-group-label">Review Content:</p>
        <p>{review.content}</p>
      </div>
      <p className="report-group-label">Reports on this review:</p>
      <div className="report-group-subreports-container">
        {props.reports.map((report, i) => {
          return (
            <SubReport
              key={report.id}
              reportId={report.id}
              reviewId={report.reviewId}
              reason={report.reason}
              timestamp={report.createdAt}
              isLast={i == props.reports.length - 1}
            />
          );
        })}
      </div>
      <div className="report-group-footer">
        <Button variant="danger" className="mr-3" onClick={props.onDeny}>
          Deny
        </Button>
        <Button variant="success" onClick={props.onAccept}>
          Accept
        </Button>
      </div>
    </div>
  );
};

export default ReportGroup;
