import React, { FC, useState } from 'react';
import Form from 'react-bootstrap/Form';
import Button from 'react-bootstrap/Button';
import './ReportForm.scss';
import Modal from 'react-bootstrap/Modal';
import trpc from '../../trpc';
import { ReportSubmission } from '@peterportal/types';
import spawnToast from '../../helpers/toastify';

interface ReportFormProps {
  showForm: boolean;
  reviewId: number;
  reviewContent: string | undefined;
  closeForm: () => void;
}

const ReportForm: FC<ReportFormProps> = (props) => {
  const [reason, setReason] = useState<string>('');
  const [busy, setBusy] = useState(false);

  const postReport = async (report: ReportSubmission) => {
    setBusy(true);
    try {
      await trpc.reports.add.mutate(report);
      spawnToast('Your report has been submitted successfully');
      props.closeForm();
    } catch {
      spawnToast('Unable to submit review', true);
    } finally {
      setBusy(false);
    }
  };

  const submitReport = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    if (reason.length === 0) return spawnToast('Report reason must not be empty', true);

    const report = { reviewId: props.reviewId, reason };
    postReport(report);
  };

  const resetForm = () => {
    setReason('');
  };

  return (
    <Modal show={props.showForm} onShow={resetForm} onHide={props.closeForm} centered className="ppc-modal report-form">
      <Modal.Header closeButton>
        <h2>Report Review</h2>
      </Modal.Header>
      <Modal.Body>
        <Form noValidate onSubmit={submitReport}>
          <Form.Label className="ppc-modal-form-label">Review Content</Form.Label>
          <p className="reported-review-content">
            <i>{props.reviewContent}</i>
          </p>

          <Form.Group>
            <Form.Label>Why are you reporting this review?</Form.Label>
            <Form.Control
              as="textarea"
              placeholder="Enter a reason..."
              minLength={1}
              maxLength={500}
              onChange={(e) => setReason(e.target.value)}
              value={reason}
              rows={4}
            />
          </Form.Group>
          <Button variant="primary" type="submit" disabled={!reason.length || busy}>
            Submit Report
          </Button>
        </Form>
      </Modal.Body>
    </Modal>
  );
};

export default ReportForm;
