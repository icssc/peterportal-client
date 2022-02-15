import React, { FC, ChangeEvent, useState, useEffect } from 'react';
import './ReviewForm.scss';
import axios from 'axios';
import { useCookies } from 'react-cookie';;
import { Icon } from 'semantic-ui-react';

import { useAppDispatch } from '../../store/hooks';
import { ReportData } from '../../types/types';

interface ReportFormProps {
    reviewID: string
    reviewContent: string
}

const ReportForm: FC<ReportFormProps> = (props) => {
    const dispatch = useAppDispatch();
    const [reason, setReason] = useState('');
    
    // const updateReason = (e: ChangeEvent) => {
    //     setReason(e.target)
    // }

    return <></>
}