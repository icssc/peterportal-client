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
  const date = new Date(Date.parse(props.timestamp));
  return (
    <div className="subreport">
      <div className="subreport-content">
        <span className="label">
          Reason for Report on {date.getMonth() + 1}/{date.getDate()}/{date.getFullYear()}
        </span>
        <i>{props.reason}</i>
      </div>
    </div>
  );
};

export default SubReport;
