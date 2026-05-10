import { useContext } from 'react';
import { Button, Dialog, DialogActions, DialogContent, Stack } from '@mui/material';
import { Apple as AppleIcon, Google as GoogleIcon } from '@mui/icons-material';
import ThemeContext from '../style/theme-context';

interface SignInDialogProps {
  open: boolean;
  onClose: () => void;
}

const SignInDialog = ({ open, onClose }: SignInDialogProps) => {
  const { darkMode } = useContext(ThemeContext);

  return (
    <Dialog
      open={open}
      onClose={onClose}
      maxWidth="xl"
      fullScreen
      PaperProps={{
        sx: {
          width: 'fit-content',
          height: 'fit-content',
          borderRadius: '0.25rem',
          width: '526px',
          fontWeight: 500,
        },
      }}
      sx={{ padding: '1rem' }}
    >
      <DialogContent>
        <Stack spacing={2}>
          <Button
            href="/planner/api/users/auth/google?provider=google"
            startIcon={<GoogleIcon />}
            color="primary"
            variant="contained"
            size="large"
            fullWidth
            sx={{ fontSize: '11.25px' }}
          >
            Sign in with Google
          </Button>
          <Button
            href="/planner/api/users/auth/google?provider=apple"
            startIcon={<AppleIcon />}
            variant="contained"
            size="large"
            fullWidth
            sx={{
              backgroundColor: darkMode ? '#fff' : '#000',
              color: darkMode ? '#000' : '#fff',
              '&:hover': {
                backgroundColor: darkMode ? '#e0e0e0' : '#333',
              },
              fontSize: '11.25px',
            }}
          >
            Sign in with Apple
          </Button>
        </Stack>
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose} color="inherit" variant="text" sx={{ fontSize: '10.5px' }}>
          Cancel
        </Button>
      </DialogActions>
    </Dialog>
  );
};

export default SignInDialog;
