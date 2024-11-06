import { FC, useCallback, useEffect, useState } from 'react';
import ReportGroup from './ReportGroup';
import './Reports.scss';
import trpc from '../../trpc';
import { ReportData } from '@peterportal/types';

const Reports: FC = () => {
  const [data, setData] = useState<ReviewDisplay[]>([]);
  const [loaded, setLoaded] = useState<boolean>(false);

  interface ReviewDisplay {
    reviewId: number;
    reports: ReportData[];
  }

  const getData = useCallback(async () => {
    const reports = await trpc.reports.get.query();

    const reportsDisplay: ReviewDisplay[] = [];

    reports.forEach((report) => {
      let i;
      if ((i = reportsDisplay.findIndex((reviewDisplay) => report.reviewId === reviewDisplay.reviewId)) < 0) {
        reportsDisplay.push({
          reviewId: report.reviewId,
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

  const acceptReports = async (reviewId: number) => {
    await trpc.reviews.delete.mutate({ id: reviewId });
    // reports are automatically deleted when deleting a review
    setData(data.filter((review) => review.reviewId !== reviewId));
  };

  const denyReports = async (reviewId: number) => {
    await trpc.reports.delete.mutate({ reviewId });
    setData(data.filter((review) => review.reviewId !== reviewId));
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
              key={review.reviewId}
              reviewId={review.reviewId}
              reports={review.reports}
              onAccept={() => acceptReports(review.reviewId)}
              onDeny={() => denyReports(review.reviewId)}
            />
          );
        })}
      </div>
    );
  }
};

export default Reports;
