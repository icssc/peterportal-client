import { useIsMobile } from '../helpers/util';
import { useState } from 'react';

import {
  Box,
  Button,
  Link,
  ListItemIcon,
  ListSubheader,
  MenuItem,
  MenuList,
  Popover,
  Stack,
  Typography,
  ButtonGroup,
} from '@mui/material';
import { EventNote, Route, UnfoldMore } from '@mui/icons-material';

import { Logo } from './Logo';

// TEMP COLOR
const BLUE = '#305db7';

export function LogoAndSwitcher() {
  const isMobile = useIsMobile();
  const [anchorEl, setAnchorEl] = useState<HTMLElement | null>(null);

  return (
    <Box>
      {isMobile ? (
        <>
          <Button
            onClick={(event) => setAnchorEl(event.currentTarget)}
            endIcon={<UnfoldMore />}
            sx={{
              paddingRight: 1,
              p: 1,
              minWidth: 'auto',
              color: 'white',
              '& .MuiTouchRipple-child': {
                borderRadius: 0.5,
                bgcolor: 'white',
              },
            }}
          >
            <Logo />
          </Button>

          <Popover
            open={!!anchorEl}
            anchorEl={anchorEl}
            onClose={() => setAnchorEl(null)}
            anchorOrigin={{ vertical: 'bottom', horizontal: 'left' }}
            transformOrigin={{ vertical: 'top', horizontal: 'left' }}
          >
            <MenuList
              subheader={
                <ListSubheader
                  component="div"
                  sx={{
                    lineHeight: '30px',
                    fontSize: '10.5px',
                    color: 'var(--mui-palette-text-secondary)',
                    bgcolor: 'inherit',
                  }}
                >
                  Switch Apps
                </ListSubheader>
              }
              sx={{ width: '200px' }}
            >
              <MenuItem
                onClick={() => setAnchorEl(null)}
                component={Link}
                href="https://staging-shared.antalmanac.com/"
                sx={{ minHeight: 'fit-content', textDecoration: 'none', color: 'inherit', height: '30px' }}
              >
                <ListItemIcon>
                  <EventNote sx={{ fontSize: '18px' }} />
                </ListItemIcon>
                <Typography
                  sx={{
                    fontSize: '12px',
                    letterSpacing: '0px',
                  }}
                >
                  Scheduler
                </Typography>
              </MenuItem>
              <MenuItem
                selected
                onClick={() => setAnchorEl(null)}
                component={Link}
                href="/planner"
                sx={{ minHeight: 'fit-content', textDecoration: 'none', color: 'inherit', height: '30px' }}
              >
                <ListItemIcon>
                  <Route sx={{ fontSize: '18px' }} />
                </ListItemIcon>
                <Typography
                  sx={{
                    fontSize: '12px',
                    letterSpacing: '0px',
                  }}
                >
                  Planner
                </Typography>
              </MenuItem>
            </MenuList>
          </Popover>
        </>
      ) : (
        <Stack direction="row" alignItems="center" gap={2}>
          <Link href={'/planner'}>
            <Logo />
          </Link>
          <ButtonGroup variant="outlined" color="inherit">
            <Button
              startIcon={<EventNote />}
              sx={{
                color: 'white',
                bgcolor: BLUE,
                fontSize: 14,
                fontWeight: 500,
                py: 0.4,
                letterSpacing: 0,
              }}
              variant="outlined"
              component={Link}
              href="https://staging-shared.antalmanac.com/"
            >
              Scheduler
            </Button>
            <Button
              startIcon={<Route />}
              sx={{
                color: BLUE,
                '&:hover': { bgcolor: 'grey.100' },
                bgcolor: 'white',
                fontSize: 14,
                fontWeight: 500,
                py: 0.4,
                letterSpacing: 0,
              }}
              variant="contained"
              component={Link}
              href="/planner"
            >
              Planner
            </Button>
          </ButtonGroup>
        </Stack>
      )}
    </Box>
  );
}
