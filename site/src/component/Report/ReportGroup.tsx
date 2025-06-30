import { FC, useEffect, useState } from 'react';
import { Button, Paper } from '@mui/material';
import DeleteIcon from '@mui/icons-material/Delete';
import PersonRemoveIcon from '@mui/icons-material/PersonRemove';
import './ReportGroup.scss';
import { ReportData, ReviewData } from '@peterportal/types';
import trpc from '../../trpc';

interface SubReportProps {
  reason: string;
  timestamp: string;
}

const SubReport: FC<SubReportProps> = ({ reason, timestamp }) => {
  const date = new Date(Date.parse(timestamp));
  const dateText = `${date.getMonth() + 1}/${date.getDate()}/${date.getFullYear()}`;
  return (
    <div className="subreport">
      <b>Reason for Report on {dateText}</b>
      <i>{reason}</i>
    </div>
  );
};

interface ReportGroupProps {
  reviewId: number;
  reports: ReportData[];
  onAccept: () => void;
  onDeny: () => void;
}

const ReportGroup: FC<ReportGroupProps> = (props) => {
  const [review, setReview] = useState<ReviewData>(null!);

  useEffect(() => {
    const getReviewData = async (reviewId: number) => {
      const reviews = await trpc.reviews.getAdminView.query({ reviewId });
      setReview(reviews[0]);
    };
    getReviewData(props.reviewId);
  }, [props.reviewId]);

  if (!review) {
    return null;
  }

  const reviewCreationDate = new Date(review.createdAt).toLocaleString('default', {
    year: 'numeric',
    month: 'long',
    day: 'numeric',
  });

  return (
    <Paper className="report-group ppc-paper">
      <div className="report-group-header">
        <div className="report-group-identifier">
          {review.courseId} {review.professorId}
        </div>
        <div className="edit-buttons">
          <Button className="ppc-mui-button" variant="contained" onClick={props.onDeny}>
            <PersonRemoveIcon /> Ignore
          </Button>
          <Button className="ppc-mui-button primary-button" variant="contained" onClick={props.onAccept}>
            <DeleteIcon /> Accept Report
          </Button>
        </div>
      </div>
      <div className="report-group-content">{review.content}</div>
      <div className="report-group-user-display">
        Posted by <i>{review.userDisplay}</i> on <i>{reviewCreationDate}</i>
      </div>
      <div className="report-group-subreports-container">
        {props.reports.map((report) => (
          <SubReport key={report.id} reason={report.reason} timestamp={report.createdAt} />
        ))}
      </div>
    </Paper>
  );
};

export default ReportGroup;
