import { FC } from 'react';
import { Snackbar, SnackbarContent } from '@mui/material';

interface ToastProps {
  text: string;
  severity: string;
  showToast: boolean;
  onClose: () => void;
}

const Toast: FC<ToastProps> = ({ text, severity, showToast, onClose }) => {
  const backgroundColor = severity === 'error' ? 'var(--red-primary)' : 'var(--blue-primary)';
  return (
    <Snackbar
      anchorOrigin={{ vertical: 'bottom', horizontal: 'right' }}
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
