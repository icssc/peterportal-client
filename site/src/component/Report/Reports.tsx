import axios, { AxiosResponse } from "axios";
import React, { FC, useEffect, useState } from "react";
import { ReportData } from "../../types/types";
import ReportGroup from "./ReportGroup";
import './Reports.scss';

interface ReportsProps {
}

const Reports: FC = () => {
    const [data, setData] = useState<ReviewDisplay[]>([]);
    const [loaded, setLoaded] = useState<boolean>(false);

    interface AdminResponse {
        admin: boolean
    }

    interface ReviewDisplay {
        reviewID: string,
        reports: ReportData[]
    }

    const getData = async () => {
        const reports: AxiosResponse<ReportData[]> = await axios.get('/reports');
        const reportsData: ReportData[] = reports.data;
        
        let reportsDisplay: ReviewDisplay[] = [];

        reportsData.forEach(report => {
            let i;
            if ((i = reportsDisplay.findIndex(reviewDisplay => report.reviewID === reviewDisplay.reviewID)) < 0) {
                reportsDisplay.push({
                    reviewID: report.reviewID,
                    reports: [report]
                });
            }
            else {
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
    }

    useEffect(() => {
        getData();
    }, []);

    const acceptReports = async (reviewID: string) => {
        const deleteReviewResponse = await axios.delete('/reviews', { data: { id: reviewID } });
        const deleteReportsResponse = await axios.delete('/reports', { data: { reviewID: reviewID } });
        setData(data.filter(review => review.reviewID !== reviewID));
    }

    const denyReports = async (reviewID: string) => {
        const deleteReportsResponse = await axios.delete('/reports', { data: { reviewID: reviewID } });
        setData(data.filter(review => review.reviewID !== reviewID));
    }

    if (!loaded) {
        return <p>Loading...</p>;
    } else if (data.length === 0) {
        return <p>No reports to display at the moment.</p>;
    } else {
        return (
            <div className='reports-container'>
                <h1>User Review Reports</h1>
                <p>Denying a review's reports will discard the reports and preserve the review.</p>
                <p>Accepting a review's reports will discard the reports and the review.</p>
                {data.map(review => {
                    return <ReportGroup
                        key={review.reviewID}
                        reviewID={review.reviewID}
                        reports={review.reports}
                        onAccept={() => acceptReports(review.reviewID)}
                        onDeny={() => denyReports(review.reviewID)} />
                })}
            </div>
        )
    }
};

export default Reports;