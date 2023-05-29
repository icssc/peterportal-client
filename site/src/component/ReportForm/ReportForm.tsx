import React, { FC, ChangeEvent, useState, useEffect } from 'react';
import axios from 'axios';
import { Icon } from 'semantic-ui-react';
import Form from 'react-bootstrap/Form';
import Badge from 'react-bootstrap/Badge';
import Row from 'react-bootstrap/Row';
import Col from 'react-bootstrap/Col';
import Button from 'react-bootstrap/Button';
import './ReportForm.scss';
import { useAppDispatch } from '../../store/hooks';
import { ReportData } from '../../types/types';
import { useCookies } from 'react-cookie';
import Modal from 'react-bootstrap/Modal';
import { isMobile } from 'react-device-detect';

interface ReportFormProps {
    showForm: boolean;
    reviewID: string | undefined;
    reviewContent: string;
    closeForm: () => void;
}

const ReportForm: FC<ReportFormProps> = (props) => {
    const dispatch = useAppDispatch();
    const [reason, setReason] = useState<string>('');
    const [reportSubmitted, setReportSubmitted] = useState<boolean>(false);

    const [validated, setValidated] = useState<boolean>(false);


    const postReport = async (report: ReportData) => {
        const res = await axios.post('/api/reports', report);
        setReportSubmitted(true);
    }

    const submitReport = (e: React.FormEvent<HTMLFormElement>) => {
        e.preventDefault();
        if (reason.length > 500) return;
        if (reason.length === 0) return;
        setValidated(true);

        const date = new Date();
        const year = date.getFullYear();
        const month = (1 + date.getMonth()).toString();
        const day = date.getDate().toString();
        const report = {
            reviewID: props.reviewID!,
            reason,
            timestamp: month + '/' + day + '/' + year,
        }
        postReport(report);
    }

    const reportForm = (
        <Form noValidate validated={validated} onSubmit={submitReport} style={{ width: '100%' }}>
            <h2 className='report-form-header'>Report Review</h2>
            <p>Does the report contain vulgar or hateful content? Submit an anonymous report here.</p>
            <div className='report-form-review-content'>
                <p className='report-form-review-content-label'>You're reporting:</p>
                <p className='report-form-review-content-text'>{props.reviewContent}</p>
            </div>
            <Form.Group className='report-form-section'>
                <Form.Label>Why are you reporting this review?</Form.Label>
                <Form.Control
                    as="textarea"
                    placeholder="Enter a reason..."
                    onChange={(e) => {
                        setReason(e.target.value);
                    }}
                />
            </Form.Group>
            <div className='d-flex justify-content-end'>
                <Button className='py-2 px-4 mr-3' variant="outline-secondary" size={isMobile ? 'sm' : undefined} onClick={props.closeForm}>Cancel</Button>
                <Button className='py-2 px-4' type="submit" variant="secondary" size={isMobile ? 'sm' : undefined}>Submit</Button>
            </div>
        </Form>
    );

    return (
        <Modal show={props.showForm} centered animation={false} onHide={props.closeForm}>
            <div className='report-form'>
                {reportSubmitted ? (
                    <div className='submitted-report-form'>
                        <Icon name='check circle' size='huge' />
                        <h1>Thank You</h1>
                        <p>Your report has been submitted successfully.</p>
                    </div>
                ) : reportForm}
            </div>
        </Modal>
    );
}

export default ReportForm;