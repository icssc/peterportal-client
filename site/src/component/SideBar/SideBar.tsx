import { useEffect, useState } from 'react';
import { NavLink } from 'react-router-dom';
import { CSSTransition } from 'react-transition-group';
import Logo from '../../asset/peterportal-banner-logo.svg';
import './Sidebar.scss';

import { useAppDispatch, useAppSelector } from '../../store/hooks';
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

  const linkInfo = [
    {
      to: '/',
      icon: <ListAltRoundedIcon />,
      fullName: 'Course Catalog',
      shortName: 'Courses',
    },
    {
      to: '/search/professors',
      icon: <PeopleOutlineIcon />,
      fullName: 'Professors',
      shortName: 'Professors',
    },
    {
      to: '/roadmap',
      icon: <MapOutlinedIcon />,
      fullName: "Peter's Roadmap",
      shortName: 'Roadmap',
    },
    ...(isAdmin
      ? [
          {
            to: '/admin/verify',
            icon: <DomainVerificationIcon />,
            fullName: 'Verify Reviews',
            shortName: 'Verification',
          },
          {
            to: '/admin/reports',
            icon: <WarningAmberIcon />,
            fullName: 'View Reports',
            shortName: 'Reports',
          },
        ]
      : []),
  ];

  const links = (
    <div className="sidebar-links">
      <ul>
        {linkInfo.map((link) => (
          <li key={link.shortName}>
            <NavLink
              to={link.to}
              className={({ isActive }) =>
                isActive || (link.shortName === 'Courses' && location.pathname === '/search/courses')
                  ? 'sidebar-active'
                  : ''
              }
              onClick={closeSidebar}
            >
              <span className="sidebar-icon">{link.icon}</span>
              <span className="full-name">{link.fullName}</span>
              <span className="short-name">{link.shortName}</span>
            </NavLink>
          </li>
        ))}
      </ul>
    </div>
  );

  return (
    <div>
      <div className="sidebar mini">{links}</div>
      <CSSTransition in={showSidebar} timeout={500} unmountOnExit>
        <UIOverlay zIndex={399} onClick={closeSidebar} />
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
    </div>
  );
};

export default SideBar;
