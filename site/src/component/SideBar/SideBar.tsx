import { useEffect, useState } from 'react';
import { NavLink } from 'react-router-dom';
import { CSSTransition } from 'react-transition-group';
import Logo from '../../asset/peterportal-banner-logo.svg';
import './Sidebar.scss';

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

const SideBar = () => {
  const dispatch = useAppDispatch();
  const showSidebar = useAppSelector((state) => state.ui.sidebarOpen);
  const isLoggedIn = useIsLoggedIn();
  const [isAdmin, setIsAdmin] = useState<boolean>(false);

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
          <NavLink
            to="/"
            className={({ isActive }) => (isActive || location.pathname === '/search/courses' ? 'sidebar-active' : '')}
            onClick={closeSidebar}
          >
            <div>
              <ListAltRoundedIcon className="sidebar-icon" />
            </div>
            <span className="full-name">Course Catalog</span>
            <span>Courses</span>
          </NavLink>
        </li>
        <li>
          <NavLink
            to="/search/professors"
            className={({ isActive }) => (isActive ? 'sidebar-active' : '')}
            onClick={closeSidebar}
          >
            <div>
              <PeopleOutlineIcon className="sidebar-icon" />
            </div>
            <span className="full-name">Professors</span>
            <span>Professors</span>
          </NavLink>
        </li>
        <li>
          <NavLink
            to="/roadmap"
            className={({ isActive }) => (isActive ? 'sidebar-active' : '')}
            onClick={closeSidebar}
          >
            <div>
              <MapOutlinedIcon className="sidebar-icon" />
            </div>
            <span className="full-name">Peter's Roadmap</span>
            <span>Roadmap</span>
          </NavLink>
        </li>
        {isAdmin && (
          <>
            <li>
              <NavLink
                to="/admin/verify"
                className={({ isActive }) => (isActive ? 'sidebar-active' : '')}
                onClick={closeSidebar}
              >
                <div>
                  <DomainVerificationIcon className="sidebar-icon" />
                </div>
                <span className="full-name">Verify Reviews</span>
                <span>Verification</span>
              </NavLink>
            </li>
            <li>
              <NavLink
                to="/admin/reports"
                className={({ isActive }) => (isActive ? 'sidebar-active' : '')}
                onClick={closeSidebar}
              >
                <div>
                  <WarningAmberIcon className="sidebar-icon" />
                </div>
                <span className="full-name">View Reports</span>
                <span>Reports</span>
              </NavLink>
            </li>{' '}
          </>
        )}
      </ul>
    </div>
  );

  return (
    <>
      <div className="sidebar mini">{links}</div>
      <CSSTransition in={showSidebar} timeout={500} unmountOnExit>
        <UIOverlay zIndex={399} onClick={closeSidebar}></UIOverlay>
      </CSSTransition>
      <CSSTransition in={showSidebar} timeout={500} unmountOnExit>
        <div className="sidebar">
          {/* Close Button */}
          <div className="button-container">
            <img alt="PeterPortal" src={Logo} height="32" />
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
