'use client';

import { IconButton, Paper, Stack, Typography, useTheme } from '@mui/material';
import EventNoteIcon from '@mui/icons-material/EventNote';
import RouteIcon from '@mui/icons-material/Route';
import Link from 'next/link';

const SideNav = () => {
  const theme = useTheme();

  return (
    <Paper
      elevation={0}
      sx={{
        height: '100%',
        width: 64,
        zIndex: '4000',
        position: 'fixed',
        top: 0,
        left: 0,
        borderRight: '2px solid var(--mui-palette-overlay-overlay2)', // @todo: use theme
      }}
    >
      <Stack direction="column" alignItems="center" gap={1}>
        <IconButton href="">
          <Stack direction="column" alignItems="center" sx={{ color: theme.palette.text.primary }}>
            <EventNoteIcon fontSize="medium" sx={{ color: 'var(--mui-palette-text-primary) ' }} />
            <Link href="" style={{ textDecoration: 'none', color: 'var(--mui-palette-text-primary)' }}>
              <Typography fontSize={11}>Scheduler</Typography>
            </Link>
          </Stack>
        </IconButton>

        <IconButton href="">
          <Stack direction="column" alignItems="center" sx={{ color: theme.palette.primary.main }}>
            <RouteIcon fontSize="medium" color="inherit" />
            <Link href="" style={{ textDecoration: 'none', color: 'inherit' }}>
              <Typography fontSize={11}>Planner</Typography>
            </Link>
          </Stack>
        </IconButton>
      </Stack>
    </Paper>
  );
};

export default SideNav;
