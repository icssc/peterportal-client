import type { FC } from 'react';

import { Button, IconButton, Stack } from '@mui/material';

import { Apple as AppleIcon, Google as GoogleIcon } from '@mui/icons-material';
import Image from 'next/image';
import MenuIcon from '@mui/icons-material/Menu';
import type { UserMetadata } from '@peterportal/types';

interface ProfileMenuButtonsProps {
  user: UserMetadata | null;
  handleOpen: (event: React.MouseEvent<HTMLElement>) => void;
}
const ProfileMenuButtons: FC<ProfileMenuButtonsProps> = ({ user, handleOpen }) => {
  if (!user) {
    return (
      <>
        <Stack direction="row" spacing={1}>
          <Button
            className="header-button"
            variant="text"
            size="medium"
            startIcon={<GoogleIcon />}
            color="inherit"
            href="/planner/api/users/auth/google?provider=google"
          >
            Sign In with Google
          </Button>
          <Button
            className="header-button"
            variant="text"
            size="medium"
            startIcon={<AppleIcon />}
            color="inherit"
            href="/planner/api/users/auth/google?provider=apple"
          >
            Sign In with Apple
          </Button>
        </Stack>
        <IconButton onClick={handleOpen} color="inherit">
          <MenuIcon />
        </IconButton>
      </>
    );
  }

  const { name, picture } = user;

  const profileButtonStyles = {
    display: 'flex',
    alignItems: 'center',
    gap: '4px',
    padding: '6px 8px',
    borderRadius: 24,
    border: 'none',
  };

  const profilePicStyles = {
    width: 24,
    height: 24,
    borderRadius: '100%',
    color: 'inherit',
    whiteSpace: 'normal',
  };

  return (
    <Button sx={profileButtonStyles} onClick={handleOpen} variant="text" color="inherit">
      <Image src={picture} alt={name} style={profilePicStyles} width={24} height={24} />
      <MenuIcon />
    </Button>
  );
};

export default ProfileMenuButtons;
