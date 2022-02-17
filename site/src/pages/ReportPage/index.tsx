import axios, { AxiosResponse } from "axios";
import React, { FC, useEffect, useState } from "react";
import { Icon } from "semantic-ui-react";
import SubReport from "src/component/Report/SubReport";
import { ReportData } from "src/types/types";
import './ReportPage.scss';

interface ReportPageProps {

}

const ReportPage: FC = () => {

    const [reports, setReports] = useState<ReportData[]>([]);
    const [loaded, setLoaded] = useState<boolean>(false);
    const [authorized, setAuthorized] = useState<boolean>(true); // TODO: check if user is admin

    interface AdminResponse {
        admin: boolean
    }

    const checkAdmin = async () => {
        const res: AxiosResponse<AdminResponse> = await axios.get('/users/isAdmin');
        const isAdmin: boolean = res.data.admin;
        if (isAdmin) {
            setAuthorized(true);
        }
    }
    
    const getReports = async () => {
        console.log('here');
        const reports: AxiosResponse<ReportData[]> = await axios.get('/reports');
        setReports(reports.data);
        
        setLoaded(true);
        
        
        // return reports.data;
    }

    useEffect(() => {
        getReports();
    }, []);

    const acceptReport = async (reportID: string | undefined, reviewID: string | undefined) => {
        const deleteReviewResponse = await axios.delete('/reviews', { data: { id: reviewID } });
        const deleteReportResponse = await axios.delete('/reports', { data: { id: reportID } });
        setReports(reports.filter(report => report._id !== reportID));
    }

    const denyReport = async (reportID: string | undefined) => {
        const deleteReportResponse = await axios.delete('/reports', { data: { id: reportID } });
        setReports(reports.filter(report => report._id !== reportID));
    }

    if (!loaded) {
        return <p>Loading...</p>;
    } else if (!authorized) {
        return (
            <div>
                <div style={{ display: 'flex', flexDirection: 'row' }}>
                    <Icon name='lock' size='large'/>
                    <h1>Access Denied</h1>
                    <h3>You are not authorized to view this page.</h3>
                </div>
            </div>
        );
    } else if (reports.length === 0) {
        return <p>No reports to display at the moment.</p>;
    } else {
        return (
            <div className='reports-container'>
                {reports.map(report => {
                    return <SubReport 
                                key={report.reviewID}
                                reportID={report._id} 
                                reviewID={report.reviewID} 
                                reason={report.reason} 
                                onAccept={() => acceptReport(report._id, report.reviewID)} 
                                onDeny={() => denyReport(report._id)} />
                })}
            </div>
        )
    }
};

export default ReportPage;