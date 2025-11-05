import React, { FC, useState } from 'react';
import './ReportForm.scss';
import Modal from 'react-bootstrap/Modal';
import trpc from '../../trpc';
import { ReportSubmission } from '@peterportal/types';
import Toast from '../../helpers/toast';
import { AlertColor } from '@mui/material';
import { Button, Box, FormControl, FormLabel, TextField } from '@mui/material';

interface ReportFormProps {
  showForm: boolean;
  reviewId: number;
  reviewContent: string | undefined;
  closeForm: () => void;
}

const ReportForm: FC<ReportFormProps> = (props) => {
  const [reason, setReason] = useState<string>('');
  const [busy, setBusy] = useState(false);
  const [showToast, setShowToast] = useState(false);
  const [toastMsg, setToastMsg] = useState('');
  const [toastSeverity, setToastSeverity] = useState('info');

  const handleClose = () => {
    setShowToast(false);
  };

  const postReport = async (report: ReportSubmission) => {
    setBusy(true);
    try {
      await trpc.reports.add.mutate(report);
      setToastMsg('Your report has been submitted successfully');
      setToastSeverity('success');
      setShowToast(true);
      props.closeForm();
    } catch {
      setToastMsg('Unable to submit review');
      setToastSeverity('error');
      setShowToast(true);
    } finally {
      setBusy(false);
    }
  };

  const submitReport = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    if (reason.length === 0) {
      setToastMsg('Report reason must not be empty');
      setToastSeverity('success');
      setShowToast(true);
      return;
    }

    const report = { reviewId: props.reviewId, reason };
    postReport(report);
  };

  const resetForm = () => setReason('');

  return (
    <Modal show={props.showForm} onShow={resetForm} onHide={props.closeForm} centered className="ppc-modal report-form">
      <Modal.Header closeButton>
        <h2>Report Review</h2>
      </Modal.Header>
      <Modal.Body>
        <Box component="form" noValidate onSubmit={submitReport}>
          <FormLabel>Review Content</FormLabel>
          <p className="reported-review-content">
            <i>{props.reviewContent}</i>
          </p>

          <FormLabel>Why are you reporting this review?</FormLabel>

          <FormControl>
            <TextField
              placeholder="Enter a reason..."
              multiline
              slotProps={{
                htmlInput: {
                  minLength: 1,
                  maxLength: 500,
                },
              }}
              onChange={(e) => setReason(e.target.value)}
              value={reason}
              rows={4}
            />
          </FormControl>

          <Button type="submit" disabled={!reason.length} loading={busy}>
            Submit Report
          </Button>
        </Box>
      </Modal.Body>
      <Toast text={toastMsg} severity={toastSeverity as AlertColor} showToast={showToast} onClose={handleClose} />
    </Modal>
  );
};

export default ReportForm;
