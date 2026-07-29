'use client';
import { FC } from 'react';

import './AppHeader.scss';
import { LogoAndSwitcher } from '../../shared-components/LogoAndSwitcher';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import Profile from './Profile';
import SearchModule from '../SearchModule/SearchModule';

import ArrowLeftIcon from '@mui/icons-material/ArrowBack';
import { IconButton } from '@mui/material';

import { useIsMobile } from '../../helpers/util';
import { setSelectedMobileTab, setShowMobileFullscreenSearch } from '../../store/slices/roadmapSlice';
import { setShowMobileCreditsMenu } from '../../store/slices/transferCreditsSlice';
import { usePathname } from 'next/navigation';

import SaveButton from './SaveButton';
import ExportButton from './Export';

const AppHeader: FC = () => {
  const dispatch = useAppDispatch();
  const isMobile = useIsMobile();
  const isShowFullscreenSearch = useAppSelector((state) => state.roadmap.showMobileFullscreenSearch);
  const isRoadmapPage = usePathname() == '/';

  const closeFullscreenSearch = () => {
    dispatch(setShowMobileFullscreenSearch(false));
    dispatch(setSelectedMobileTab(0));
    dispatch(setShowMobileCreditsMenu(false));
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
            <SearchModule index="courses" autoFocusInput />
          </div>
        </div>
      </header>
    );

  return (
    <header className={`navbar ${isMobile ? 'mobile' : 'desktop'}`}>
      <div className="navbar-nav">
        <div className="navbar-left">
          <LogoAndSwitcher />
        </div>

        {/* Search */}
        {isRoadmapPage && (
          <>
            <ExportButton />
            <SaveButton />
          </>
        )}
        <Profile />
      </div>
    </header>
  );
};

export default AppHeader;
