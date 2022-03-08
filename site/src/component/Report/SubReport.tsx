import React, { FC, useEffect, useState } from "react";
import axios, { AxiosResponse } from "axios";
import { ReviewData } from "src/types/types";
import Button from 'react-bootstrap/Button';
import './SubReport.scss';

interface SubReportProps {
    reportID: string,
    reviewID: string,
    reason: string,
    // onAccept: () => void,
    // onDeny: () => void
}


const SubReport: FC<SubReportProps> = (props) => {
    // const [review, setReview] = useState<ReviewData>(null!);

    // const getReviewData = async (reviewID: string) => {
    //     const res: AxiosResponse<ReviewData[]> = await axios.get(`/reviews?reviewID=${reviewID}`);
    //     const review: ReviewData = res.data[0];
    //     console.log(review);
    //     setReview(review);
    // }

    // useEffect(() => {
    //     getReviewData(props.reviewID!);
    // }, []);

    return (
        <div className='subreport'>
            {/* <div className='subreport-identifier'>
                <div className="subreport-professor-name">
                    {review.professorID}
                </div>
                <div className='subreport-user-display'>
                    Posted by {review.userDisplay} on {new Date(review.timestamp).toLocaleString('default', { year: 'numeric', month: 'long', day: 'numeric' })}
                </div>
            </div> */}
            {/* <div className='subreport-content'>
                <label>
                    Review Content:
                </label>
                <p>{review.reviewContent}</p>
            </div> */}
            <div className='subreport-content'>
                {/* <label>
                    Report Reason:
                </label> */}
                <p>{props.reason}</p>
            </div>
            
        </div>
    );
}

export default SubReport;