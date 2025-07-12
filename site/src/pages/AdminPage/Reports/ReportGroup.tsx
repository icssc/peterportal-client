import { FC, useEffect, useState } from 'react';
import { Button, Card } from '@mui/material';
import DeleteIcon from '@mui/icons-material/Delete';
import PersonRemoveIcon from '@mui/icons-material/PersonRemove';
import SubReport from './SubReport';
import './ReportGroup.scss';
import { ReportData, ReviewData } from '@peterportal/types';
import trpc from '../../../trpc';

interface ReportGroupProps {
  reviewId: number;
  reports: ReportData[];
  onAccept: () => void;
  onDeny: () => void;
}

const ReportGroup: FC<ReportGroupProps> = (props) => {
  const [review, setReview] = useState<ReviewData>(null!);

  const getReviewData = async (reviewId: number) => {
    const review = (await trpc.reviews.getAdminView.query({ reviewId }))[0];
    setReview(review);
  };

  useEffect(() => {
    getReviewData(props.reviewId);
  }, [props.reviewId]);

  if (!review) {
    return <></>;
  } else {
    return (
      <Card className="report-group ppc-card">
        <div className="report-group-header">
          <h3 className="report-group-identifier">
            {review.courseId} {review.professorId}
          </h3>
          <div className="edit-buttons">
            <Button className="ppc-mui-button" variant="text" onClick={props.onDeny} startIcon={<PersonRemoveIcon />}>
              Ignore
            </Button>
            <Button
              className="ppc-mui-button primary-button"
              variant="text"
              onClick={props.onAccept}
              startIcon={<DeleteIcon />}
            >
              Accept Report
            </Button>
          </div>
        </div>
        <div className="report-group-content">{review.content}</div>
        <div className="report-group-user-display">
          Posted by <i>{review.userDisplay}</i> on{' '}
          <i>
            {new Date(review.createdAt).toLocaleString('default', { year: 'numeric', month: 'long', day: 'numeric' })}
          </i>
        </div>
        <div className="report-group-subreports-container">
          {props.reports.map((report, i) => (
            <SubReport
              key={report.id}
              reportId={report.id}
              reviewId={report.reviewId}
              reason={report.reason}
              timestamp={report.createdAt}
              isLast={i == props.reports.length - 1}
            />
          ))}
        </div>
      </Card>
    );
  }
};

export default ReportGroup;
