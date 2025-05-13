import { FC } from 'react';
import './SubReport.scss';

interface SubReportProps {
  reportId: number;
  reviewId: number;
  reason: string;
  timestamp: string;
  isLast: boolean;
}

const SubReport: FC<SubReportProps> = (props) => {
  return (
    <div className="subreport">
      <div className="subreport-content">
        <h5>Reported at {props.timestamp}</h5>
        <p>{props.reason}</p>
      </div>
      {!props.isLast && <br />}
    </div>
  );
};

export default SubReport;
