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
import { useCookies } from 'react-cookie';;



interface ReportFormProps {
    reviewID: string | undefined
    reviewContent: string
    closeForm: () => void
}

const ReportForm: FC<ReportFormProps> = (props) => {
    const dispatch = useAppDispatch();
    const [reason, setReason] = useState<string>('');
    const [reportSubmitted, setReportSubmitted] = useState<boolean>(false);

    const [validated, setValidated] = useState<boolean>(false);


    const postReport = async (report: ReportData) => {
        const res = await axios.post('/reports', report);
        setReportSubmitted(true);
    }

    const submitReport = (e: React.FormEvent<HTMLFormElement>) => {
        e.preventDefault();
        if (reason.length > 500) return;
        if (reason.length === 0) return;
        setValidated(true);
        
        const report = {
            reviewID: props.reviewID!,
            reason
        }
        postReport(report);
    }

    const reportForm = (
        <Form noValidate validated={validated} onSubmit={submitReport}>
            <Row>
                <p>{props.reviewContent}</p>
            </Row>
            <Row>
                <Form.Group className='report-form-section'>
                    <Form.Label>Why are you reporting this review?</Form.Label>
                    <Form.Control
                    as="textarea"
                    placeholder="Enter a reason..."
                    style={{ height: '15vh', width: '100%' }}
                    onChange={(e) => {
                        setReason(e.target.value);
                        // if (overCharLimit && e.target.value.length < 500) {
                        //   setOverCharLimit(false)
                        // }
                    }}
                    />
                </Form.Group>
            </Row>
            <Row>
                <Col className='mb-3'>
                    <Button className='py-2 px-4 float-right' type="submit" variant="secondary">Submit</Button>
                    <Button className='py-2 px-4 mr-3 float-right' variant="outline-secondary" onClick={props.closeForm}>Cancel</Button>
                </Col>
          </Row>
        </Form>
    );

    return (
        <div className='report-form'>
            {reportSubmitted ? (
                <div className='submitted-report-form'>
                <Icon name='check circle' size='huge' />
                <h1>Thank You</h1>
                <p>Your report has been submitted successfully.</p>
                </div>
            ) : reportForm}
        </div>
    );
}

export default ReportForm;