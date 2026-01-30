import { FC } from 'react';
import { Snackbar, SnackbarContent } from '@mui/material';

export type ToastSeverity = 'error' | 'success' | 'info';

interface ToastProps {
  text: string;
  severity: ToastSeverity;
  showToast: boolean;
  onClose: () => void;
}

const Toast: FC<ToastProps> = ({ text, severity, showToast, onClose }) => {
  const backgroundColor = severity === 'error' ? 'var(--mui-palette-error-main)' : 'var(--mui-palette-primary-main)';
  return (
    <Snackbar
      anchorOrigin={{ vertical: 'bottom', horizontal: 'left' }}
      open={showToast}
      autoHideDuration={3000}
      onClose={onClose}
      message={text}
    >
      <SnackbarContent
        message={text}
        sx={{
          backgroundColor,
          color: 'white',
        }}
      />
    </Snackbar>
  );
};

export default Toast;
