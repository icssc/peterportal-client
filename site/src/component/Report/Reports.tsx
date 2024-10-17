import { FC, useCallback, useEffect, useState } from 'react';
import ReportGroup from './ReportGroup';
import './Reports.scss';
import trpc from '../../trpc';
import { ReportData } from '@peterportal/types';

const Reports: FC = () => {
  const [data, setData] = useState<ReviewDisplay[]>([]);
  const [loaded, setLoaded] = useState<boolean>(false);

  interface ReviewDisplay {
    reviewID: string;
    reports: ReportData[];
  }

  const getData = useCallback(async () => {
    const reports = await trpc.reports.get.query();

    const reportsDisplay: ReviewDisplay[] = [];

    reports.forEach((report) => {
      let i;
      if ((i = reportsDisplay.findIndex((reviewDisplay) => report.reviewID === reviewDisplay.reviewID)) < 0) {
        reportsDisplay.push({
          reviewID: report.reviewID,
          reports: [report],
        });
      } else {
        reportsDisplay[i].reports.push(report);
      }
    });

    reportsDisplay.sort((rd1, rd2) => {
      if (rd1.reports.length > rd2.reports.length) return -1;
      if (rd1.reports.length < rd2.reports.length) return 1;
      return 0;
    });

    setData(reportsDisplay);
    setLoaded(true);
  }, []);

  useEffect(() => {
    getData();
    document.title = 'View Reports | PeterPortal';
  }, [getData]);

  const acceptReports = async (reviewID: string) => {
    await trpc.reviews.delete.mutate({ id: reviewID });
    // reports are automatically deleted when deleting a review
    setData(data.filter((review) => review.reviewID !== reviewID));
  };

  const denyReports = async (reviewID: string) => {
    await trpc.reports.delete.mutate({ reviewID: reviewID });
    setData(data.filter((review) => review.reviewID !== reviewID));
  };

  if (!loaded) {
    return <p>Loading...</p>;
  } else if (data.length === 0) {
    return <p>No reports to display at the moment.</p>;
  } else {
    return (
      <div className="reports-container">
        <h1>User Review Reports</h1>
        <p>Denying a review's reports will discard the reports and preserve the review.</p>
        <p>Accepting a review's reports will discard the reports and the review.</p>
        {data.map((review) => {
          return (
            <ReportGroup
              key={review.reviewID}
              reviewID={review.reviewID}
              reports={review.reports}
              onAccept={() => acceptReports(review.reviewID)}
              onDeny={() => denyReports(review.reviewID)}
            />
          );
        })}
      </div>
    );
  }
};

export default Reports;
