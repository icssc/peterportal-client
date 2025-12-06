'use client';
import { FC } from 'react';

import Logo from '../../asset/peterportal-banner-logo.svg';
import Image from 'next/image';
import './AppHeader.scss';

import { useAppDispatch, useAppSelector } from '../../store/hooks';
import Profile from './Profile';
import PPCOverlayTrigger from '../PPCOverlayTrigger/PPCOverlayTrigger';
import SearchModule from '../SearchModule/SearchModule';

import GitHubIcon from '@mui/icons-material/GitHub';
import SearchIcon from '@mui/icons-material/Search';
import ArrowLeftIcon from '@mui/icons-material/ArrowBack';
import Home from '@mui/icons-material/Home';
import TuneIcon from '@mui/icons-material/Tune';
import SmsIcon from '@mui/icons-material/Sms';
import { Button, IconButton } from '@mui/material';
import Link from 'next/link';
import { useIsMobile } from '../../helpers/util';
import { setShowMobileFullscreenSearch, setShowMobileSearchFilters } from '../../store/slices/roadmapSlice';
import { usePathname } from 'next/navigation';

const AppHeader: FC = () => {
  const dispatch = useAppDispatch();
  const isMobile = useIsMobile();
  const isShowFullscreenSearch = useAppSelector((state) => state.roadmap.showMobileFullscreenSearch);
  const isShowMobileSearchFilters = useAppSelector((state) => state.roadmap.showMobileSearchFilters);
  const isRoadmapPage = usePathname() == '/';
  const courseSearchQuery = useAppSelector((state) => state.search['courses'].query);

  const showFullscreenSearch = () => {
    dispatch(setShowMobileFullscreenSearch(true));
  };
  const closeFullscreenSearch = () => {
    dispatch(setShowMobileFullscreenSearch(false));
    // also hide filters when closing fullscreen search
    dispatch(setShowMobileSearchFilters(false));
  };

  const toggleMobileSearchFilters = () => {
    dispatch(setShowMobileSearchFilters(!isShowMobileSearchFilters));
  };

  const popover = (
    <div className="popover-body">
      <h4>Beta Disclaimer</h4>
      <p>
        Please note that this is a beta version of PeterPortal, which is still undergoing development. Some content on
        this web application may not be accurate. Users are encouraged to double check details.
      </p>
      <p>
        Should you encounter any bugs, glitches, lack of functionality or other problems on the application, please let
        us know immediately so we can rectify these accordingly. Your help in this regard is greatly appreciated.
      </p>
      <div className="feedback">
        <Button
          color="inherit"
          href="https://github.com/icssc/peterportal-client/issues/new/choose"
          target="_blank"
          rel="noopener noreferrer"
          startIcon={<GitHubIcon />}
        >
          Report an issue
        </Button>
        <Button
          color="inherit"
          href="https://form.asana.com/?k=4h9ZTRkVUT9ZwfJrmvxDDw&d=1208267282546207"
          target="_blank"
          rel="noopener noreferrer"
          startIcon={<SmsIcon />}
        >
          Feedback
        </Button>
      </div>
    </div>
  );

  if (isMobile && isShowFullscreenSearch && isRoadmapPage)
    return (
      <header className="navbar">
        <div className="navbar-nav">
          <div className="navbar-left">
            <IconButton onClick={closeFullscreenSearch}>
              <ArrowLeftIcon />
            </IconButton>
          </div>
          <div className="fullscreen-search-row">
            <div>
              <SearchModule index="courses" />
            </div>
            {courseSearchQuery && (
              <IconButton onClick={toggleMobileSearchFilters}>
                <TuneIcon />
              </IconButton>
            )}
          </div>
        </div>
      </header>
    );

  return (
    <header className="navbar">
      <div className="navbar-nav">
        <div className="navbar-left">
          {/* Search */}
          {isMobile && isRoadmapPage && (
            <IconButton onClick={showFullscreenSearch}>
              <SearchIcon />
            </IconButton>
          )}
          {!isRoadmapPage && (
            <IconButton href="/">
              <Home />
            </IconButton>
          )}
        </div>

        {/* Logo */}
        <div className="navbar-logo">
          <Link href="/">
            <Image alt="PeterPortal" id="peterportal-logo" src={Logo} />
          </Link>
        </div>

        <div style={{ display: 'flex', alignItems: 'center', height: '100%' }}>
          <div className="beta" style={{ margin: 'auto 12px' }}>
            <PPCOverlayTrigger popoverContent={popover} placement="bottom">
              <div id="beta-tag">
                <div>beta</div>
                <div className="shade">v1.2</div>
              </div>
            </PPCOverlayTrigger>
          </div>
          <Profile />
        </div>
      </div>
    </header>
  );
};

export default AppHeader;
