import { FC } from 'react';
import { Card } from '@mui/material';
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
    <Card className="ppc-card subreport">
      <div className="subreport-content">
        <span className="label">
          Reason for Report on {date.getMonth() + 1}/{date.getDate()}/{date.getFullYear()}
        </span>
        <i>{props.reason}</i>
      </div>
    </Card>
  );
};

export default SubReport;
