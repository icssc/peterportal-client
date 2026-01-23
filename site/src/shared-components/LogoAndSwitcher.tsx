import { useIsMobile } from '../helpers/util';
import { useState } from 'react';

import MapOutlinedIcon from '@mui/icons-material/MapOutlined';
import UnfoldMoreIcon from '@mui/icons-material/UnfoldMore';
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
import { EventNote, Route } from '@mui/icons-material';

import { Logo } from './Logo';

// TEMP COLOR
const BLUE = '#305db7';

export function LogoAndSwitcher() {
  const isMobile = useIsMobile();
  const [anchorEl, setAnchorEl] = useState<HTMLElement | null>(null);

  return isMobile ? (
    // Mobile
    // @TODO Match with staging 1411's mobile dropdown
    // @TODO fix mobile logo sizing
    <Box>
      <Button onClick={(event) => setAnchorEl(event.currentTarget)} size="small" sx={{ paddingRight: 1.5 }}>
        <Stack gap={1} direction="row" alignItems="center">
          {/* <Image src={currentLogo.mobileLogo} alt="logo" title={currentLogo.attribution} height={36} width={78} /> */}
          <Logo />
          <UnfoldMoreIcon />
        </Stack>
      </Button>

      <Popover
        open={!!anchorEl}
        anchorEl={anchorEl}
        onClose={() => setAnchorEl(null)}
        anchorOrigin={{
          vertical: 'bottom',
          horizontal: 'left',
        }}
        transformOrigin={{
          vertical: 'top',
          horizontal: 'left',
        }}
      >
        <MenuList
          subheader={
            <ListSubheader component="div" sx={{ lineHeight: '30px' }}>
              Switch Apps
            </ListSubheader>
          }
          sx={{ width: '200px' }}
        >
          <MenuItem onClick={() => setAnchorEl(null)} href="https://antalmanac.com" component="a">
            <ListItemIcon>
              <EventNote />
            </ListItemIcon>
            <Typography>Scheduler</Typography>
          </MenuItem>
          <MenuItem selected onClick={() => setAnchorEl(null)} href="/" component={Link}>
            <ListItemIcon>
              <MapOutlinedIcon />
            </ListItemIcon>
            <Typography>Planner</Typography>
          </MenuItem>
        </MenuList>
      </Popover>
    </Box>
  ) : (
    // Desktop
    <Box>
      <Stack direction="row" alignItems="center" gap={2}>
        <Logo />
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
          >
            Planner
          </Button>
        </ButtonGroup>
      </Stack>
    </Box>
  );
}
