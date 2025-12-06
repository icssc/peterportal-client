'use client';
import { FC } from 'react';

import Logo from '../../asset/peterportal-banner-logo.svg';
import Image from 'next/image';
import './AppHeader.scss';

import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setSidebarStatus } from '../../store/slices/uiSlice';
import Profile from './Profile';

import MenuIcon from '@mui/icons-material/Menu';
import { IconButton } from '@mui/material';

import Link from 'next/link';

const AppHeader: FC = () => {
  const dispatch = useAppDispatch();
  const sidebarOpen = useAppSelector((state) => state.ui.sidebarOpen);

  const toggleMenu = () => {
    dispatch(setSidebarStatus(!sidebarOpen));
  };

  return (
    <header className="navbar">
      <div className="navbar-nav">
        <div className="navbar-left">
          {/* Hamburger Menu */}
          <IconButton className="navbar-menu-btn" onClick={toggleMenu}>
            <MenuIcon className="navbar-menu-icon" />
          </IconButton>
        </div>

        {/* Logo */}
        <div className="navbar-logo">
          <Link href="/">
            <Image alt="PeterPortal" id="peterportal-logo" src={Logo} />
          </Link>
        </div>

        <Profile />
      </div>
    </header>
  );
};

export default AppHeader;
