import { FC, useCallback, useEffect, useState } from 'react';
import ReportGroup from './ReportGroup';
import ReviewItemGrid from '../../component/ReviewItemGrid/ReviewItemGrid';
import './Reports.scss';
import trpc from '../../trpc';
import { ReportData } from '@peterportal/types';

interface ReviewDisplay {
  reviewId: number;
  reports: ReportData[];
}

const ReportsList: FC<{
  data: ReviewDisplay[];
  acceptReports: (reviewId: number) => Promise<void>;
  denyReports: (reviewId: number) => Promise<void>;
}> = ({ data, acceptReports, denyReports }) => {
  return (
    <ReviewItemGrid>
      {data.length == 0 && <span>There are currently no reports that need attention</span>}
      {data.map((reviewPair) => (
        <div key={'report-' + reviewPair.reviewId}>
          <ReportGroup
            key={reviewPair.reviewId}
            reviewId={reviewPair.reviewId}
            reports={reviewPair.reports}
            onAccept={() => acceptReports(reviewPair.reviewId)}
            onDeny={() => denyReports(reviewPair.reviewId)}
          />
        </div>
      ))}
    </ReviewItemGrid>
  );
};

const Reports: FC = () => {
  const [data, setData] = useState<ReviewDisplay[]>([]);
  const [reportsLoading, setReportsLoading] = useState(true);

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
    setReportsLoading(false);
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

  return (
    <div className="content-wrapper reports-container">
      <h1>User Review Reports</h1>
      <p>
        Accepting a report will <b>delete</b> the review. Ignoring a report will <b>preserve</b> the review.
      </p>
      {reportsLoading ? (
        <p>Loading...</p>
      ) : (
        <ReportsList data={data} acceptReports={acceptReports} denyReports={denyReports} />
      )}
    </div>
  );
};

export default Reports;
