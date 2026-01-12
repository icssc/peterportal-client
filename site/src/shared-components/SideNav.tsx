'use client';

import { IconButton, Paper, Stack, SvgIcon, Tooltip, Typography } from '@mui/material';
import EventNoteIcon from '@mui/icons-material/EventNote';
import RouteIcon from '@mui/icons-material/Route';
import FeedbackIcon from '@mui/icons-material/Feedback';
import Link from 'next/link';
import { useIsMobile } from '../helpers/util';
import { FC } from 'react';
import ICSSCLogo from './IcsscLogo';

interface SidenavTooltipProps {
  text: string;
  children: JSX.Element;
}
const SidenavTooltip: FC<SidenavTooltipProps> = ({ children, text }) => {
  const offset = [0, -8];
  const modifiers = [{ name: 'offset', options: { offset } }];
  const slotProps = { popper: { modifiers } };

  return (
    <Tooltip title={text} placement="right" slotProps={slotProps}>
      {children}
    </Tooltip>
  );
};

const SideNav = () => {
  const isMobile = useIsMobile();

  const paperStyleOverrides = {
    height: '100%',
    width: 64,
    zIndex: 300,
    position: 'fixed',
    top: 0,
    left: 0,
    borderRight: '1px solid var(--mui-palette-divider)',
    borderRadius: 0,
  };

  if (isMobile) {
    return null;
  }

  return (
    <Paper elevation={0} sx={paperStyleOverrides}>
      <Stack direction="column" alignItems="center" sx={{ paddingBlock: '8px 4px' }} height="100%" gap={4}>
        <Link
          href="https://antalmanac.com"
          style={{ textDecoration: 'none', color: 'var(--mui-palette-text-primary)' }}
        >
          <Stack direction="column" alignItems="center">
            <EventNoteIcon fontSize="medium" />
            <Typography fontSize={11}>Scheduler</Typography>
          </Stack>
        </Link>

        <Link href="/" style={{ textDecoration: 'none', color: 'var(--mui-palette-secondary-main)' }}>
          <Stack direction="column" alignItems="center">
            <RouteIcon fontSize="medium" />
            <Typography fontSize={11}>Planner</Typography>
          </Stack>
        </Link>

        <Stack style={{ marginTop: 'auto' }}>
          <SidenavTooltip text="Feedback">
            <IconButton href="https://form.asana.com/?k=lUbtptdrBT28EDCN3sbtKw&d=1208267282546207" target="_blank">
              <FeedbackIcon fontSize="medium" />
            </IconButton>
          </SidenavTooltip>

          <SidenavTooltip text="Made with <3 by ICSSC Projects">
            <IconButton href="https://studentcouncil.ics.uci.edu/projects" target="_blank">
              <SvgIcon inheritViewBox component={ICSSCLogo} />
            </IconButton>
          </SidenavTooltip>
        </Stack>
      </Stack>
    </Paper>
  );
};

export default SideNav;
