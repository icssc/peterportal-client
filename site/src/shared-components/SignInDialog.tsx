import { useContext } from 'react';
import { Button, Dialog, DialogContent, DialogTitle, Stack } from '@mui/material';
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
      PaperProps={{
        sx: {
          width: 'fit-content',
          minWidth: 320,
          borderRadius: '0.75rem',
          padding: '0.5rem',
        },
      }}
    >
      <DialogTitle sx={{ textAlign: 'center', fontWeight: 600 }}>Sign In</DialogTitle>
      <DialogContent>
        <Stack spacing={1.5}>
          <Button
            href="/planner/api/users/auth/google?provider=google"
            startIcon={<GoogleIcon />}
            color="primary"
            variant="contained"
            size="large"
            fullWidth
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
            }}
          >
            Sign in with Apple
          </Button>
        </Stack>
      </DialogContent>
    </Dialog>
  );
};

export default SignInDialog;
