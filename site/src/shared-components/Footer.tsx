import { Button, Container, Stack, useTheme } from '@mui/material';
import { FC } from 'react';
import GitHubIcon from '@mui/icons-material/GitHub';
import UpdateIcon from '@mui/icons-material/Update';
import { setChangelogOpen } from '../store/slices/uiSlice';
import { useAppDispatch } from '../store/hooks';

const Footer: FC<{ className?: string }> = ({ className }) => {
  const theme = useTheme();
  const dispatch = useAppDispatch();

  const openChangelog = () => {
    dispatch(setChangelogOpen(true));
  };

  const stackStyleOverrides = {
    color: theme.palette.text.secondary,
    gap: 2,
    justifyContent: 'center',
  };

  return (
    <Container className={className}>
      <Stack direction="row" sx={stackStyleOverrides}>
        <Button
          variant="text"
          size="large"
          color="inherit"
          startIcon={<GitHubIcon />}
          href="https://github.com/icssc/peterportal-client"
          target="_blank"
        >
          GitHub
        </Button>
        <Button variant="text" size="large" color="inherit" startIcon={<UpdateIcon />} onClick={openChangelog}>
          What&rsquo;s New
        </Button>
      </Stack>
    </Container>
  );
};

export default Footer;
