import Image, { type StaticImageData } from 'next/image';
import { useIsMobile } from '../helpers/util';

import DefaultLogo from '../asset/logo.svg';
import MobileDefaultLogo from '../asset/mobile-logo.svg';

import ChristmasLogo from '../asset/christmas-logo.png';
import MobileChristmasLogo from '../asset/christmas-mobile-logo.png';

import ThanksgivingLogo from '../asset/thanksgiving-logo.png';
import MobileThanksgivingLogo from '../asset/thanksgiving-mobile-logo.png';

import HalloweenLogo from '../asset/halloween-logo.png';
import MobileHalloweenLogo from '../asset/halloween-mobile-logo.png';

import EventNoteIcon from '@mui/icons-material/EventNote';
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
} from '@mui/material';
import { useState } from 'react';
import { usePathname, useRouter } from 'next/navigation';

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
  desktopLogo: DefaultLogo,
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

export function LogoAndSwitcher() {
  const isMobile = useIsMobile();
  const [anchorEl, setAnchorEl] = useState<HTMLElement | null>(null);
  const router = useRouter();

  const currentLogo = logos.find(isCurrentSeason) ?? defaultLogo;
  const isRoadmapPage = usePathname() === '/';

  // manually navigate to the roadmap page because MenuItem and next <Link/>
  // don't play well with each other (hard reload occurs)
  const navToRoadmap = () => {
    setAnchorEl(null);
    router.push('/');
  };

  if (!isMobile)
    return (
      <Link href="/">
        <Image
          src={currentLogo.desktopLogo}
          alt="logo"
          title={currentLogo?.attribution}
          style={{
            width: 'auto',
            height: '32px',
          }}
        />
      </Link>
    );

  return (
    <Box>
      <Button onClick={(event) => setAnchorEl(event.currentTarget)} size="small" sx={{ paddingRight: 1.5 }}>
        <Stack gap={1} direction="row" alignItems="center">
          <Image src={currentLogo.mobileLogo} alt="logo" title={currentLogo.attribution} height={32} />
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
              <EventNoteIcon />
            </ListItemIcon>
            <Typography>Scheduler</Typography>
          </MenuItem>
          <MenuItem selected={isRoadmapPage} onClickCapture={navToRoadmap}>
            <ListItemIcon>
              <MapOutlinedIcon />
            </ListItemIcon>
            <Typography>Planner</Typography>
          </MenuItem>
        </MenuList>
      </Popover>
    </Box>
  );
}
