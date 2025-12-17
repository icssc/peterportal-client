'use client';

import { Paper, Stack, Typography } from '@mui/material';
import EventNoteIcon from '@mui/icons-material/EventNote';
import RouteIcon from '@mui/icons-material/Route';
import Link from 'next/link';
import { useIsMobile } from '../helpers/util';

const SideNav = () => {
  const isMobile = useIsMobile();

  const paperStyleOverrides = {
    height: '100%',
    width: 64,
    zIndex: 300,
    position: 'fixed',
    top: 0,
    left: 0,
    borderRight: '1px solid #60616680',
    borderRadius: 0,
  };

  if (isMobile) {
    return null;
  }

  return (
    <Paper elevation={0} sx={paperStyleOverrides}>
      <Stack direction="column" alignItems="center" sx={{ gap: '16px', paddingTop: '8px' }}>
        <Link
          href="https://antalmanac.com"
          style={{ textDecoration: 'none', color: 'var(--mui-palette-text-primary)' }}
        >
          <Stack direction="column" alignItems="center">
            <EventNoteIcon fontSize="medium" />
            <Typography fontSize={11}>Scheduler</Typography>
          </Stack>
        </Link>

        <Link href="/" style={{ textDecoration: 'none', color: 'var(--mui-palette-primary-main)' }}>
          <Stack direction="column" alignItems="center">
            <RouteIcon fontSize="medium" />
            <Typography fontSize={11}>Planner</Typography>
          </Stack>
        </Link>
      </Stack>
    </Paper>
  );
};

export default SideNav;
