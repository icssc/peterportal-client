import { FC, useEffect, useState } from 'react';
import { NavLink } from 'react-router-dom';
import { Icon } from 'semantic-ui-react';
import { XCircle } from 'react-bootstrap-icons';
import { useCookies } from 'react-cookie';
import './Sidebar.scss';
import DefaultAvatar from '../../asset/default-avatar.png';
import { Button } from 'react-bootstrap';

import { useAppSelector, useAppDispatch } from '../..//store/hooks';
import { setSidebarStatus } from '../../store/slices/uiSlice';
import axios, { AxiosResponse } from 'axios';

const SideBar: FC = () => {
  const dispatch = useAppDispatch();
  const showSidebar = useAppSelector((state) => state.ui.sidebarOpen);
  const [cookies] = useCookies(['user']);
  const [name, setName] = useState('');
  const [picture, setPicture] = useState('');
  const [isAdmin, setIsAdmin] = useState<boolean>(false);

  const isLoggedIn = cookies.user !== undefined;

  interface AdminResponse {
    admin: boolean;
  }

  const checkAdmin = async () => {
    const res: AxiosResponse<AdminResponse> = await axios.get('/api/users/isAdmin');
    const admin = res.data.admin;
    setIsAdmin(admin);
  };

  useEffect(() => {
    // if the user is logged in
    if (isLoggedIn) {
      setName(cookies.user.name);
      setPicture(cookies.user.picture);
    }
    checkAdmin();
  });

  const closeSidebar = () => dispatch(setSidebarStatus(false));

  const links = (
    <div className="sidebar-links">
      <ul>
        <li>
          <NavLink
            to="/"
            className={({ isActive }) =>
              isActive || location.pathname.match(/^\/search\/(courses|professors)$/) ? 'sidebar-active' : ''
            }
            onClick={closeSidebar}
          >
            <div>
              <Icon name="list alternate outline" size="large" />
            </div>
            <span>Catalogue</span>
          </NavLink>
        </li>
        <li>
          <NavLink
            to="/roadmap"
            className={({ isActive }) => (isActive ? 'sidebar-active' : '')}
            onClick={closeSidebar}
          >
            <div>
              <Icon name="map outline" size="large" />
            </div>
            <span>Peter's Roadmap</span>
          </NavLink>
        </li>
        {isLoggedIn && (
          <li>
            <NavLink
              to="/reviews"
              className={({ isActive }) => (isActive ? 'sidebar-active' : '')}
              onClick={closeSidebar}
            >
              <div>
                <Icon name="sticky note outline" size="large" />
              </div>
              <span>Reviews</span>
            </NavLink>
          </li>
        )}
        {isAdmin && (
          <>
            <li>
              <NavLink
                to="/admin/verify"
                className={({ isActive }) => (isActive ? 'sidebar-active' : '')}
                onClick={closeSidebar}
              >
                <div>
                  <Icon name="check" size="large" />
                </div>
                <span>Verify Reviews</span>
              </NavLink>
            </li>
            <li>
              <NavLink
                to="/admin/reports"
                className={({ isActive }) => (isActive ? 'sidebar-active' : '')}
                onClick={closeSidebar}
              >
                <div>
                  <Icon name="exclamation triangle" size="large" />
                </div>
                <span>View Reports</span>
              </NavLink>
            </li>{' '}
          </>
        )}
      </ul>
    </div>
  );

  if (!showSidebar) {
    return <div className="sidebar mini">{links}</div>;
  }
  return (
    <div className="sidebar">
      {/* Close Button */}
      <div className="sidebar-close">
        <XCircle className="sidebar-close-icon" onClick={closeSidebar} />
      </div>

      {/* Profile Icon and Name */}
      <div className="sidebar-profile">
        <img src={picture ? picture : DefaultAvatar} />
        <p>{name ? name : 'Anonymous Peter'}</p>
      </div>

      {/* Links */}
      {links}

      {/* Login/Logout */}
      <div className="sidebar-login">
        {isLoggedIn && (
          <a href={`/api/users/logout`}>
            <Button variant="light">
              <span className="sidebar-login-icon">
                <Icon name="sign out" className="sidebar-login-icon" />
              </span>
              Log Out
            </Button>
          </a>
        )}
        {!isLoggedIn && (
          <a href={`/api/users/auth/google`}>
            <Button variant="light">
              <span className="sidebar-login-icon">
                <Icon name="sign in" className="sidebar-login-icon" />
              </span>
              Log In
            </Button>
          </a>
        )}
      </div>
    </div>
  );
};

export default SideBar;
