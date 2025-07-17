import { FC, useCallback, useEffect, useState } from 'react';
import ReportGroup from './ReportGroup';
import ReviewItemGrid from '../../../component/ReviewItemGrid/ReviewItemGrid';
import trpc from '../../../trpc';
import { ReportData } from '@peterportal/types';

interface ReviewDisplay {
  reviewId: number;
  reports: ReportData[];
}

const Reports: FC = () => {
  const [data, setData] = useState<ReviewDisplay[]>([]);
  const [reportsLoading, setReportsLoading] = useState(true);

  const getData = useCallback(async () => {
    const reports = await trpc.reports.get.query();

    const reportsDisplay: ReviewDisplay[] = [];

    reports.forEach((report) => {
      const i = reportsDisplay.findIndex((reviewDisplay) => report.reviewId === reviewDisplay.reviewId);
      if (i < 0) {
        reportsDisplay.push({
          reviewId: report.reviewId,
          reports: [report],
        });
      } else {
        reportsDisplay[i].reports.push(report);
      }
    });

    reportsDisplay.sort((a, b) => b.reports.length - a.reports.length);

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
    <ReviewItemGrid
      title="User Review Reports"
      description="Accepting a report will delete the review. Ignoring a report will preserve the review."
      isLoading={reportsLoading}
      noDataMsg="There are currently no reports that need attention."
    >
      {data.map((reviewPair) => (
        <ReportGroup
          key={`report-${reviewPair.reviewId}`}
          reviewId={reviewPair.reviewId}
          reports={reviewPair.reports}
          onAccept={() => acceptReports(reviewPair.reviewId)}
          onDeny={() => denyReports(reviewPair.reviewId)}
        />
      ))}
    </ReviewItemGrid>
  );
};

export default Reports;
