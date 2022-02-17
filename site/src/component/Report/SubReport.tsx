import React, { FC, useEffect, useState } from "react";
import axios, { AxiosResponse } from "axios";
import { ReviewData } from "src/types/types";
import './SubReport.scss';

interface SubReportProps {
    reportID: string | undefined,
    reviewID: string | undefined,
    reason: string,
    onAccept: () => void,
    onDeny: () => void
}


const SubReport: FC<SubReportProps> = (props) => {
    const [reviewContent, setReviewContent] = useState<string>('');
    const [profName, setProfName] = useState<string>('');
    const [userDisplay, setUserDisplay] = useState<string>('');
    
    const getReviewData = async (reviewID: string) => {
        const res: AxiosResponse<ReviewData[]> = await axios.get(`/reviews?reviewID=${reviewID}`);
        const review: ReviewData = res.data[0];
        
        setReviewContent(review.reviewContent);
        setUserDisplay(review.userDisplay);
        setProfName(review.professorID);
    }

    useEffect(() => {
        getReviewData(props.reviewID!);
    }, []);

    return (
        <div className='subreport'>
            <div className='subreport-identifier'>
                <div className="subreport-professor-name">
                    {profName}
                </div>
                <div className='subreport-user-display'>
                    Reviewed by {userDisplay}
                </div>
            </div>
            <div className='subreport-review-content'>
                <p>{reviewContent}</p>
            </div>
            <div className='subreport-report-reason'>
                <label>
                    Reason given for report:
                </label>
                <p>{props.reason}</p>
            </div>
            <div className='subreport-footer'>
                <button className='subreport-reject-button' onClick={props.onDeny}>Deny</button>
                <button className='subreport-accept-button' onClick={props.onAccept}>Accept</button>
            </div>
        </div>
    );
}

export default SubReport;