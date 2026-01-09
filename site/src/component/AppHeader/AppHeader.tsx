'use client';
import { FC, useState } from 'react';

import './AppHeader.scss';
import { Logo } from '../../shared-components/Logo';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import Profile from './Profile';
import SearchModule from '../SearchModule/SearchModule';

import SearchIcon from '@mui/icons-material/Search';
import ArrowLeftIcon from '@mui/icons-material/ArrowBack';
import { CalendarMonth, Map, UnfoldMore } from '@mui/icons-material';
import {
  Box,
  Divider,
  IconButton,
  ListItemIcon,
  ListSubheader,
  MenuItem,
  MenuList,
  Popover,
  Stack,
  Typography,
} from '@mui/material';

import Link from 'next/link';
import { useIsMobile } from '../../helpers/util';
import { setShowMobileFullscreenSearch } from '../../store/slices/roadmapSlice';
import { usePathname } from 'next/navigation';

import SaveButton from './SaveButton';

const AppHeader: FC = () => {
  const dispatch = useAppDispatch();
  const isMobile = useIsMobile();
  const isShowFullscreenSearch = useAppSelector((state) => state.roadmap.showMobileFullscreenSearch);
  const isRoadmapPage = usePathname() == '/';
  const [anchorEl, setAnchorEl] = useState<HTMLElement | null>(null);
  const platform = 'Planner' as 'Planner' | 'Scheduler';

  const showFullscreenSearch = () => {
    dispatch(setShowMobileFullscreenSearch(true));
  };
  const closeFullscreenSearch = () => {
    dispatch(setShowMobileFullscreenSearch(false));
  };

  if (isMobile && isShowFullscreenSearch && isRoadmapPage)
    return (
      <header className="navbar mobile">
        <div className="navbar-nav">
          <div className="navbar-left">
            <IconButton onClick={closeFullscreenSearch} color="inherit">
              <ArrowLeftIcon />
            </IconButton>
          </div>
          <div className="fullscreen-search-row">
            <SearchModule index="courses" />
          </div>
        </div>
      </header>
    );

  return (
    <header className={`navbar ${isMobile ? 'mobile' : 'desktop'}`}>
      <div className="navbar-nav">
        <div className="navbar-left">
          <Box sx={{ display: 'flex', alignItems: 'center', gap: { xs: 0, sm: 2 } }}>
            {!isMobile && (
              <Box sx={{ display: 'flex', alignItems: 'center', gap: 2 }}>
                <Link href="/">
                  <Logo />
                </Link>

                <Divider
                  orientation="vertical"
                  flexItem
                  sx={(theme) => ({ borderColor: theme.palette.common.white })}
                />
              </Box>
            )}

            <Stack sx={{ flexDirection: 'row', alignItems: 'center' }}>
              {isMobile ? (
                <Link href="/">
                  <Logo />
                </Link>
              ) : (
                <Typography variant="h5" sx={{ minWidth: 'auto' }}>
                  {platform}
                </Typography>
              )}

              <IconButton
                onClick={(event) => setAnchorEl(event.currentTarget)}
                sx={(theme) => ({
                  borderRadius: theme.spacing(0.5),
                  paddingX: theme.spacing(0.5),
                  '& .MuiTouchRipple-child': {
                    borderRadius: theme.spacing(0.5),
                  },
                })}
              >
                <UnfoldMore />
              </IconButton>

              <Popover
                open={Boolean(anchorEl)}
                anchorEl={anchorEl}
                onClose={() => setAnchorEl(null)}
                anchorOrigin={{ vertical: 'bottom', horizontal: 'left' }}
                transformOrigin={{ vertical: 'top', horizontal: 'left' }}
              >
                <MenuList
                  subheader={
                    <ListSubheader component="div" sx={{ lineHeight: '30px' }}>
                      Switch Apps
                    </ListSubheader>
                  }
                  sx={{ width: '200px' }}
                >
                  <Link href="https://antalmanac.com" style={{ textDecoration: 'none', color: 'inherit' }}>
                    <MenuItem selected={platform === 'Scheduler'} onClick={() => setAnchorEl(null)}>
                      <ListItemIcon>
                        <CalendarMonth />
                      </ListItemIcon>
                      <Typography variant="h6">Scheduler</Typography>
                    </MenuItem>
                  </Link>
                  <Link href="/" style={{ textDecoration: 'none', color: 'inherit' }}>
                    <MenuItem selected={platform === 'Planner'} onClick={() => setAnchorEl(null)}>
                      <ListItemIcon>
                        <Map />
                      </ListItemIcon>
                      <Typography variant="h6">Planner</Typography>
                    </MenuItem>
                  </Link>
                </MenuList>
              </Popover>
            </Stack>
          </Box>
        </div>
        {/* Search */}
        {isRoadmapPage && (
          <>
            <SaveButton />
            {isMobile && (
              <IconButton onClick={showFullscreenSearch} color="inherit">
                <SearchIcon />
              </IconButton>
            )}
          </>
        )}
        <Profile />
      </div>
    </header>
  );
};

export default AppHeader;
