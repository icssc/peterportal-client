import React, { FC, useEffect, useState } from "react";
import { Divider } from 'semantic-ui-react';
import './SubReport.scss';

interface SubReportProps {
    reportID: string,
    reviewID: string,
    reason: string,
    timestamp: string;
    isLast: boolean
}


const SubReport: FC<SubReportProps> = (props) => {
    return (
        <div className='subreport'>
            <div className='subreport-content'>
                <h5>Reported at {props.timestamp}</h5>
                <p>{props.reason}</p>
            </div>
            {!props.isLast && <Divider />}
        </div>
    );
}

export default SubReport;