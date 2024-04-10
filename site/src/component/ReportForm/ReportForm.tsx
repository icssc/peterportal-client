import React, { FC, useState } from 'react';
import { Icon } from 'semantic-ui-react';
import Form from 'react-bootstrap/Form';
import Button from 'react-bootstrap/Button';
import './ReportForm.scss';
import { ReportData } from '../../types/types';
import Modal from 'react-bootstrap/Modal';
import trpc from '../../trpc';

interface ReportFormProps {
  showForm: boolean;
  reviewID: string | undefined;
  reviewContent: string;
  closeForm: () => void;
}

const ReportForm: FC<ReportFormProps> = (props) => {
  const [reason, setReason] = useState<string>('');
  const [reportSubmitted, setReportSubmitted] = useState<boolean>(false);

  const [validated, setValidated] = useState<boolean>(false);

  const postReport = async (report: ReportData) => {
    await trpc.reports.add.mutate(report);
    setReportSubmitted(true);
  };

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
    };
    postReport(report);
  };

  const reportForm = (
    <Form noValidate validated={validated} onSubmit={submitReport} style={{ width: '100%' }}>
      <h2 className="report-form-header">Report Review</h2>
      <p>Does the report contain vulgar or hateful content? Submit an anonymous report here.</p>
      <div className="report-form-review-content">
        <p className="report-form-review-content-label">You're reporting:</p>
        <p className="report-form-review-content-text">{props.reviewContent}</p>
      </div>
      <Form.Group className="report-form-section">
        <Form.Label>Why are you reporting this review?</Form.Label>
        <Form.Control
          as="textarea"
          placeholder="Enter a reason..."
          onChange={(e) => {
            setReason(e.target.value);
          }}
        />
      </Form.Group>
      <div className="d-flex justify-content-end">
        <Button className="py-2 px-4 mr-3" variant="outline-secondary" onClick={props.closeForm}>
          Cancel
        </Button>
        <Button className="py-2 px-4" type="submit" variant="secondary">
          Submit
        </Button>
      </div>
    </Form>
  );

  return (
    <Modal show={props.showForm} centered animation={false} onHide={props.closeForm}>
      <div className="report-form">
        {reportSubmitted ? (
          <div className="submitted-report-form">
            <Icon name="check circle" size="huge" />
            <h1>Thank You</h1>
            <p>Your report has been submitted successfully.</p>
          </div>
        ) : (
          reportForm
        )}
      </div>
    </Modal>
  );
};

export default ReportForm;
