'use client';
import { useState, useEffect, FC, useContext } from 'react';

import Logo from '../../asset/peterportal-banner-logo.svg';
import Image from 'next/image';
import './AppHeader.scss';

import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setSidebarStatus } from '../../store/slices/uiSlice';
import Profile from './Profile';
import trpc from '../../trpc';
import { Button, Popover } from 'react-bootstrap';
// import PPCOverlayTrigger from '../PPCOverlayTrigger/PPCOverlayTrigger';
import ThemeContext from '../../style/theme-context';

import GitHubIcon from '@mui/icons-material/GitHub';
import MenuIcon from '@mui/icons-material/Menu';
import SmsIcon from '@mui/icons-material/Sms';
import { IconButton } from '@mui/material';
import Link from 'next/link';
import dynamic from 'next/dynamic';

const PPCOverlayTrigger = dynamic(() => import('../PPCOverlayTrigger/PPCOverlayTrigger'), { ssr: false });

const AppHeader: FC = () => {
  const dispatch = useAppDispatch();
  const sidebarOpen = useAppSelector((state) => state.ui.sidebarOpen);
  const [week, setWeek] = useState('');
  const { darkMode } = useContext(ThemeContext);
  const buttonVariant = darkMode ? 'dark' : 'light';

  useEffect(() => {
    // Get the current week data
    trpc.schedule.currentWeek.query().then((res) => {
      /** @todo make this less code-smelly */
      setWeek(res.display.split(' • ')[0]);
    });
  }, []);

  const toggleMenu = () => {
    dispatch(setSidebarStatus(!sidebarOpen));
  };

  const popover = (
    <Popover.Content>
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
          as="a"
          href="https://github.com/icssc-projects/peterportal-client/issues/new"
          target="_blank"
          rel="noopener noreferrer"
          variant={buttonVariant}
        >
          <GitHubIcon /> Report an issue
        </Button>
        <Button
          as="a"
          href="https://form.asana.com/?k=4h9ZTRkVUT9ZwfJrmvxDDw&d=1208267282546207"
          target="_blank"
          rel="noopener noreferrer"
          variant={buttonVariant}
        >
          <SmsIcon /> Feedback
        </Button>
      </div>
    </Popover.Content>
  );

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

        {/* Week */}
        <div style={{ display: 'flex', alignItems: 'center', height: '100%' }}>
          <div className="beta" style={{ margin: 'auto 12px' }}>
            <PPCOverlayTrigger popoverContent={popover} placement="bottom">
              <div id="beta-tag">
                <div>beta</div>
                <div className="shade">v1.2</div>
              </div>
            </PPCOverlayTrigger>
          </div>
          <p className="school-term" style={{ height: '1rem', lineHeight: '1rem' }}>
            {week}
          </p>
          <Profile />
        </div>
      </div>
    </header>
  );
};

export default AppHeader;
