import { FC } from 'react';
import { Snackbar } from '@mui/material';
import Alert from '@mui/material/Alert';
import type { AlertColor } from '@mui/material/Alert';

interface ToastProps {
  text: string;
  severity: AlertColor;
  showToast: boolean;
  onClose: () => void;
}

const Toast: FC<ToastProps> = ({ text, severity, showToast, onClose }) => {
  return (
    <Snackbar
      anchorOrigin={{ vertical: 'bottom', horizontal: 'right' }}
      open={showToast}
      autoHideDuration={3000}
      onClose={onClose}
    >
      <Alert onClose={onClose} severity={severity} variant="filled">
        {text}
      </Alert>
    </Snackbar>
  );
};

export default Toast;
