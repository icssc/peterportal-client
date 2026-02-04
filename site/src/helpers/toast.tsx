import { FC, Fragment } from 'react';
import { Snackbar, SnackbarContent, IconButton } from '@mui/material';
import { Close } from '@mui/icons-material';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';
import ErrorIcon from '@mui/icons-material/Error';
import InfoIcon from '@mui/icons-material/Info';
import Slide from '@mui/material/Slide';

export type ToastSeverity = 'error' | 'success' | 'info';

interface ToastProps {
  text: string;
  severity: ToastSeverity;
  showToast: boolean;
  onClose: () => void;
}

const iconSx = {
  mx: '8px',
  fontSize: 20,
};

const iconMap: Record<ToastSeverity, () => JSX.Element> = {
  success: () => <CheckCircleIcon sx={iconSx} />,
  error: () => <ErrorIcon sx={iconSx} />,
  info: () => <InfoIcon sx={iconSx} />,
};

const Toast: FC<ToastProps> = ({ text, severity, showToast, onClose }) => {
  let backgroundColor;

  switch (severity) {
    case 'error':
      backgroundColor = 'var(--mui-palette-error-main)';
      break;
    case 'success':
      backgroundColor = 'var(--mui-palette-success-main)';
      break;
    default:
      backgroundColor = 'var(--mui-palette-primary-main)';
  }

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
            {iconMap[severity]()}
            {text}
          </>
        }
        sx={{
          backgroundColor,
          color: 'white',
          fontSize: '0.675rem',
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
