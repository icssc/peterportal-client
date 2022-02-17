import axios, { AxiosResponse } from "axios";
import React, { FC, useEffect, useState } from "react";
import SubReport from "src/component/Report/SubReport";
import Error from '../../component/Error/Error';
import { ReportData } from "src/types/types";
import './ReportPage.scss';

interface ReportPageProps {
}

const ReportPage: FC = () => {

    const [reports, setReports] = useState<ReportData[]>([]);
    const [loaded, setLoaded] = useState<boolean>(false);
    const [authorized, setAuthorized] = useState<boolean>(false);

    interface AdminResponse {
        admin: boolean
    }

    // user has to be authenticated as admin to view this page
    const checkAdmin = async () => {
        const res: AxiosResponse<AdminResponse> = await axios.get('/users/isAdmin');
        const isAdmin: boolean = res.data.admin;
        setAuthorized(isAdmin);
        if (isAdmin) {
            getReports();
        }
        else {
            setLoaded(true);
        }
    }

    const getReports = async () => {
        const reports: AxiosResponse<ReportData[]> = await axios.get('/reports');
        setReports(reports.data);
        setLoaded(true);
    }

    useEffect(() => {
        checkAdmin();
    }, []);

    const acceptReport = async (reportID: string, reviewID: string) => {
        const deleteReviewResponse = await axios.delete('/reviews', { data: { id: reviewID } });
        const deleteReportResponse = await axios.delete('/reports', { data: { id: reportID } });
        setReports(reports.filter(report => report._id !== reportID));
    }

    const denyReport = async (reportID: string) => {
        const deleteReportResponse = await axios.delete('/reports', { data: { id: reportID } });
        setReports(reports.filter(report => report._id !== reportID));
    }

    if (!loaded) {
        return <p>Loading...</p>;
    } else if (!authorized) {
        return (
            <Error message='Access Denied: You are not authorized to view this page.'></Error>
        );
    } else if (reports.length === 0) {
        return <p>No reports to display at the moment.</p>;
    } else {
        return (
            <div className='reports-container'>
                <h1>User Review Reports</h1>
                <p>Denying a report will discard the report and preserve the review.</p>
                <p>Accepting a report will discard the report, the review, and all other reports on the review.</p>
                {reports.map(report => {
                    return <SubReport
                        key={report.reviewID}
                        reportID={report._id!}
                        reviewID={report.reviewID}
                        reason={report.reason}
                        onAccept={() => acceptReport(report._id!, report.reviewID)}
                        onDeny={() => denyReport(report._id!)} />
                })}
            </div>
        )
    }
};

export default ReportPage;