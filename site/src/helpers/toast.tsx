import { FC, Fragment } from 'react';
import { Close } from '@mui/icons-material';
import { Snackbar, SnackbarContent, IconButton } from '@mui/material';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';
import Slide from '@mui/material/Slide';

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
      slots={{ transition: Slide }}
      slotProps={{
        transition: {
          direction: 'right',
        },
      }}
    >
      <SnackbarContent
        message={
          <>
            <CheckCircleIcon
              sx={{
                mx: '8px',
                fontSize: 20,
              }}
            />
            {text}
          </>
        }
        sx={{
          backgroundColor,
          color: 'white',
          fontSize: '0.675rem',
          lineHeight: 1.43,
          letterSpacing: '0.01071em',
          alignItems: 'center',
          borderRadius: '4px',
        }}
        action={
          <Fragment>
            <IconButton aria-label="close" color="inherit" onClick={onClose}>
              <Close sx={{ fontSize: 20 }} />
            </IconButton>
          </Fragment>
        }
      />
    </Snackbar>
  );
};

export default Toast;
