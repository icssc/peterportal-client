import Image, { type StaticImageData } from 'next/image';
import { useIsMobile } from '../helpers/util';

// import DefaultLogo from '../asset/logo.svg';
import MobileDefaultLogo from '../asset/mobile-logo.svg';
import NewDefaultLogo from '../asset/mobile-logo-cropped.svg'; // @TODO rename when done

import ChristmasLogo from '../asset/christmas-logo.png';
import MobileChristmasLogo from '../asset/christmas-mobile-logo.png';

import ThanksgivingLogo from '../asset/thanksgiving-logo.png';
import MobileThanksgivingLogo from '../asset/thanksgiving-mobile-logo.png';

import HalloweenLogo from '../asset/halloween-logo.png';
import MobileHalloweenLogo from '../asset/halloween-mobile-logo.png';

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

import { useState } from 'react';

// TEMP COLOR
const BLUE = '#305db7';

type Logo = {
  name: string;
  desktopLogo: StaticImageData;
  mobileLogo: StaticImageData;
  startDay: number;
  startMonthIndex: number;
  endDay: number;
  endMonthIndex: number;
  attribution?: string;
};

const defaultLogo: Logo = {
  name: 'Default',
  desktopLogo: NewDefaultLogo,
  mobileLogo: MobileDefaultLogo,
  startDay: 0,
  startMonthIndex: 0,
  endDay: 31,
  endMonthIndex: 11,
};

const logos: Logo[] = [
  {
    name: 'Christmas',
    desktopLogo: ChristmasLogo,
    mobileLogo: MobileChristmasLogo,
    startDay: 1,
    startMonthIndex: 11,
    endDay: 31,
    endMonthIndex: 11,
    attribution: 'Thanks Aejin for designing this seasonal logo!',
  },
  {
    name: 'Thanksgiving',
    desktopLogo: ThanksgivingLogo,
    mobileLogo: MobileThanksgivingLogo,
    startDay: 1,
    startMonthIndex: 10,
    endDay: 30,
    endMonthIndex: 10,
    attribution: 'Thanks Aejin for designing this seasonal logo!',
  },
  {
    name: 'Halloween',
    desktopLogo: HalloweenLogo,
    mobileLogo: MobileHalloweenLogo,
    startDay: 1,
    startMonthIndex: 9,
    endDay: 31,
    endMonthIndex: 9,
    attribution: 'Thanks Aejin for designing this seasonal logo!',
  },
  defaultLogo,
];

function isCurrentSeason(logo: Logo) {
  const now = new Date();
  const year = now.getFullYear();

  const start = new Date(year, logo.startMonthIndex, logo.startDay);
  const end = new Date(year, logo.endMonthIndex, logo.endDay);

  return now >= start && now <= end;
}

export function Logo() {
  const isMobile = useIsMobile();
  const currentLogo = logos.find(isCurrentSeason) ?? defaultLogo;

  return (
    <Image
      src={isMobile ? currentLogo.mobileLogo : currentLogo.desktopLogo}
      alt="logo"
      title={currentLogo?.attribution}
      height={32}
      width={isMobile ? 48 : 78}
    />
  );
}

export function LogoAndSwitcher() {
  const isMobile = useIsMobile();
  const [anchorEl, setAnchorEl] = useState<HTMLElement | null>(null);

  const currentLogo = logos.find(isCurrentSeason) ?? defaultLogo;

  return isMobile ? (
    // Mobile
    // @TODO Match with staging 1411's mobile dropdown
    // @TODO fix mobile logo sizing
    <Box>
      <Button onClick={(event) => setAnchorEl(event.currentTarget)} size="small" sx={{ paddingRight: 1.5 }}>
        <Stack gap={1} direction="row" alignItems="center">
          <Image src={currentLogo.mobileLogo} alt="logo" title={currentLogo.attribution} height={36} width={78} />
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
    // @TODO fix font and letter sizing and switcher height
    <Box>
      <Stack direction="row" alignItems="center" gap={2}>
        <Logo />
        <ButtonGroup variant="outlined" color="inherit" sx={{ height: 32 }}>
          <Button
            startIcon={<EventNote />}
            sx={{
              color: 'white',
              bgcolor: BLUE,
              fontSize: 14,
              fontWeight: 500,
              py: 0.2,
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
              py: 0.2,
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
