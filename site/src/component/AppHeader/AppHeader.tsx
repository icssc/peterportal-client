'use client';
import { FC } from 'react';

import './AppHeader.scss';
import { Logo } from './Logo';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import Profile from './Profile';
import SearchModule from '../SearchModule/SearchModule';

import SearchIcon from '@mui/icons-material/Search';
import ArrowLeftIcon from '@mui/icons-material/ArrowBack';
import { IconButton } from '@mui/material';

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

  const showFullscreenSearch = () => {
    dispatch(setShowMobileFullscreenSearch(true));
  };
  const closeFullscreenSearch = () => {
    dispatch(setShowMobileFullscreenSearch(false));
  };

  if (isMobile && isShowFullscreenSearch && isRoadmapPage)
    return (
      <header className="navbar">
        <div className="navbar-nav">
          <div className="navbar-left">
            <IconButton onClick={closeFullscreenSearch}>
              <ArrowLeftIcon sx={{ color: '#FFFFFF' }} />
            </IconButton>
          </div>
          <div className="fullscreen-search-row">
            <SearchModule index="courses" />
          </div>
        </div>
      </header>
    );

  return (
    <header className="navbar">
      <div className="navbar-nav">
        <div className="navbar-left">
          {/* Logo */}
          <Link href="/">
            <Logo />
          </Link>
        </div>
        {/* Search */}
        {isRoadmapPage && (
          <>
            <SaveButton />
            {isMobile && (
              <IconButton onClick={showFullscreenSearch}>
                <SearchIcon sx={{ color: '#FFFFFF' }} />
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
