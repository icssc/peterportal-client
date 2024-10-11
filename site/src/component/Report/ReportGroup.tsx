import { FC, useEffect, useState } from 'react';
import axios, { AxiosResponse } from 'axios';
import Button from 'react-bootstrap/Button';
import { ReviewData } from '../../types/types';
import SubReport from './SubReport';
import './ReportGroup.scss';
import { ReportData } from '@peterportal/types';

interface ReportGroupProps {
  reviewID: string;
  reports: ReportData[];
  onAccept: () => void;
  onDeny: () => void;
}

const ReportGroup: FC<ReportGroupProps> = (props) => {
  const [review, setReview] = useState<ReviewData>(null!);

  const getReviewData = async (reviewID: string) => {
    const res: AxiosResponse<ReviewData[]> = await axios.get(`/api/reviews?reviewID=${reviewID}`);
    const review: ReviewData = res.data[0];
    setReview(review);
  };

  useEffect(() => {
    getReviewData(props.reviewID!);
  }, []);

  if (!review) {
    return <></>;
  } else {
    return (
      <div className="report-group">
        <div className="report-group-identifier">
          <div className="report-group-professor-name">{review.professorID}</div>
          <div className="report-group-course-id">{review.courseID}</div>
          <div className="report-group-user-display">
            Posted by {review.userDisplay} on{' '}
            {new Date(review.timestamp).toLocaleString('default', { year: 'numeric', month: 'long', day: 'numeric' })}
          </div>
        </div>
        <div className="report-group-content">
          <p className="report-group-label">Review Content:</p>
          <p>{review.reviewContent}</p>
        </div>
        <p className="report-group-label">Reports on this review:</p>
        <div className="report-group-subreports-container">
          {props.reports.map((report, i) => {
            return (
              <SubReport
                key={report._id}
                reportID={report._id!}
                reviewID={report.reviewID}
                reason={report.reason}
                timestamp={report.timestamp}
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
  }
};

export default ReportGroup;
