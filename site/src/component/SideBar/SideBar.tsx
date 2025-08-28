'use client';
import { useEffect, useRef, useState } from 'react';

import { CSSTransition } from 'react-transition-group';
import Logo from '../../asset/peterportal-banner-logo.svg';
import './Sidebar.scss';
import Image from 'next/image';

import { useAppDispatch, useAppSelector } from '../..//store/hooks';
import { setSidebarStatus } from '../../store/slices/uiSlice';
import Footer from '../Footer/Footer';
import trpc from '../../trpc';
import { useIsLoggedIn } from '../../hooks/isLoggedIn';
import UIOverlay from '../UIOverlay/UIOverlay';

import ListAltRoundedIcon from '@mui/icons-material/ListAltRounded';
import DomainVerificationIcon from '@mui/icons-material/DomainVerification';
import WarningAmberIcon from '@mui/icons-material/WarningAmber';
import MapOutlinedIcon from '@mui/icons-material/MapOutlined';
import PeopleOutlineIcon from '@mui/icons-material/PeopleOutline';
import CloseIcon from '@mui/icons-material/Close';
import { IconButton } from '@mui/material';
import Link from 'next/link';
import { usePathname } from 'next/navigation';

const SideBar = () => {
  const dispatch = useAppDispatch();
  const showSidebar = useAppSelector((state) => state.ui.sidebarOpen);
  const isLoggedIn = useIsLoggedIn();
  const [isAdmin, setIsAdmin] = useState<boolean>(false);
  const pathname = usePathname();
  const sidebarRef = useRef(null);
  const overlayRef = useRef(null);

  useEffect(() => {
    if (isLoggedIn) {
      // useEffect's function is not allowed to be async, create async checkAdmin function within
      const checkAdmin = async () => {
        const { isAdmin } = await trpc.users.get.query();
        setIsAdmin(isAdmin);
      };
      checkAdmin();
    }
  }, [isLoggedIn]);

  const closeSidebar = () => dispatch(setSidebarStatus(false));

  const links = (
    <div className="sidebar-links">
      <ul>
        <li>
          <Link
            href="/"
            className={pathname === '/' || pathname === '/search/courses' ? 'sidebar-active' : ''}
            onClick={closeSidebar}
          >
            <div>
              <ListAltRoundedIcon className="sidebar-icon" />
            </div>
            <span className="full-name">Course Catalog</span>
            <span>Courses</span>
          </Link>
        </li>
        <li>
          <Link
            href="/search/professors"
            className={pathname === '/search/professors' ? 'sidebar-active' : ''}
            onClick={closeSidebar}
          >
            <div>
              <PeopleOutlineIcon className="sidebar-icon" />
            </div>
            <span className="full-name">Professors</span>
            <span>Professors</span>
          </Link>
        </li>
        <li>
          <Link href="/roadmap" className={pathname === '/roadmap' ? 'sidebar-active' : ''} onClick={closeSidebar}>
            <div>
              <MapOutlinedIcon className="sidebar-icon" />
            </div>
            <span className="full-name">Peter's Roadmap</span>
            <span>Roadmap</span>
          </Link>
        </li>
        {isAdmin && (
          <>
            <li>
              <Link
                href="/admin/verify"
                className={pathname === '/admin/verify' ? 'sidebar-active' : ''}
                onClick={closeSidebar}
              >
                <div>
                  <DomainVerificationIcon className="sidebar-icon" />
                </div>
                <span className="full-name">Verify Reviews</span>
                <span>Verification</span>
              </Link>
            </li>
            <li>
              <Link
                href="/admin/reports"
                className={pathname === '/admin/reports' ? 'sidebar-active' : ''}
                onClick={closeSidebar}
              >
                <div>
                  <WarningAmberIcon className="sidebar-icon" />
                </div>
                <span className="full-name">View Reports</span>
                <span>Reports</span>
              </Link>
            </li>{' '}
          </>
        )}
      </ul>
    </div>
  );

  return (
    <>
      <div className="sidebar mini">{links}</div>
      <CSSTransition in={showSidebar} timeout={500} unmountOnExit nodeRef={overlayRef}>
        <UIOverlay zIndex={399} onClick={closeSidebar} ref={overlayRef}></UIOverlay>
      </CSSTransition>
      <CSSTransition in={showSidebar} timeout={500} unmountOnExit nodeRef={sidebarRef}>
        <div className="sidebar" ref={sidebarRef}>
          {/* Close Button */}
          <div className="button-container">
            <Image alt="PeterPortal" src={Logo} height="32" />
            <IconButton onClick={closeSidebar} className="sidebar-close">
              <CloseIcon />
            </IconButton>
          </div>

          {links}

          <Footer />
        </div>
      </CSSTransition>
    </>
  );
};

export default SideBar;
