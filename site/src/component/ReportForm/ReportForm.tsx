import React, { FC, useState } from 'react';
import { Icon } from 'semantic-ui-react';
import Form from 'react-bootstrap/Form';
import Button from 'react-bootstrap/Button';
import './ReportForm.scss';
import Modal from 'react-bootstrap/Modal';
import trpc from '../../trpc';
import { ReportSubmission } from '@peterportal/types';

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

  const postReport = async (report: ReportSubmission) => {
    await trpc.reports.add.mutate(report);
    setReportSubmitted(true);
    setValidated(false);
  };

  const submitReport = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    if (reason.length === 0) return;
    setValidated(true);

    const report = {
      reviewID: props.reviewID!,
      reason,
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
          minLength={1}
          maxLength={500}
          isValid={reason.length > 0}
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
