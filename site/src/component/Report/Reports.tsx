import { FC, useEffect, useState } from 'react';
import ReportGroup from './ReportGroup';
import ReviewGridTemplate from '../ReviewGridTemplate/ReviewGridTemplate';
import trpc from '../../trpc';
import { ReportGroupData } from '@peterportal/types';

const Reports: FC = () => {
  const [data, setData] = useState<ReportGroupData[]>([]);
  const [reportsLoading, setReportsLoading] = useState(true);

  useEffect(() => {
    const getData = async () => {
      const reports = await trpc.reports.get.query();

      const reportGroupMap: Record<string, ReportGroupData> = {};

      reports.forEach((report) => {
        if (reportGroupMap[report.reviewId]) {
          reportGroupMap[report.reviewId].reports.push(report);
        } else {
          reportGroupMap[report.reviewId] = { reviewId: report.reviewId, reports: [report] };
        }
      });

      const reportGroups = Object.values(reportGroupMap).sort((a, b) => b.reports.length - a.reports.length);

      setData(reportGroups);
      setReportsLoading(false);
    };
    getData();
    document.title = 'View Reports | PeterPortal';
  }, []);

  const removeReviewFromData = (reviewId: number) => {
    setData(data.filter((reportGroup) => reportGroup.reviewId !== reviewId));
  };

  const acceptReports = async (reviewId: number) => {
    await trpc.reviews.delete.mutate({ id: reviewId });
    // reports are automatically deleted when deleting a review
    removeReviewFromData(reviewId);
  };

  const denyReports = async (reviewId: number) => {
    await trpc.reports.delete.mutate({ reviewId });
    removeReviewFromData(reviewId);
  };

  return (
    <ReviewGridTemplate
      title="User Review Reports"
      description="Accepting a report will delete the review. Ignoring a report will preserve the review."
      isLoading={reportsLoading}
      noDataMsg="There are currently no reports that need attention."
    >
      {data.map((reportGroup) => (
        <ReportGroup
          key={`report-${reportGroup.reviewId}`}
          reportGroup={reportGroup}
          onAccept={() => acceptReports(reportGroup.reviewId)}
          onDeny={() => denyReports(reportGroup.reviewId)}
        />
      ))}
    </ReviewGridTemplate>
  );
};

export default Reports;
