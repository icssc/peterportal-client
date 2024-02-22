import { useContext, useEffect, useState } from 'react';
import { XCircle } from 'react-bootstrap-icons';
import { useCookies } from 'react-cookie';
import { NavLink } from 'react-router-dom';
import { Icon } from 'semantic-ui-react';
import defaultAvatarDark from '../../asset/default-avatar-dark.png';
import { Button, Dropdown } from 'react-bootstrap';
import defaultAvatarLight from '../../asset/default-avatar.png';
import './Sidebar.scss';

import axios, { AxiosResponse } from 'axios';
import { useAppDispatch, useAppSelector } from '../..//store/hooks';
import { setSidebarStatus } from '../../store/slices/uiSlice';
import ThemeContext from '../../style/theme-context';

const SideBar = () => {
  const dispatch = useAppDispatch();
  const showSidebar = useAppSelector((state) => state.ui.sidebarOpen);
  const [cookies] = useCookies(['user']);
  const [name, setName] = useState('');
  const [picture, setPicture] = useState('');
  const [isAdmin, setIsAdmin] = useState<boolean>(false);
  const { darkMode, setTheme, usingSystemTheme } = useContext(ThemeContext);
  const defaultAvatar = darkMode ? defaultAvatarDark : defaultAvatarLight;

  const isLoggedIn = cookies.user !== undefined;

  interface AdminResponse {
    admin: boolean;
  }

  useEffect(() => {
    // if the user is logged in
    if (isLoggedIn) {
      setName(cookies.user.name);
      setPicture(cookies.user.picture);
      // useEffect's function is not allowed to be async, create async checkAdmin function within
      const checkAdmin = async () => {
        const res: AxiosResponse<AdminResponse> = await axios.get('/api/users/isAdmin');
        const admin = res.data.admin;
        setIsAdmin(admin);
      };
      checkAdmin();
    }
  }, [cookies.user, isLoggedIn]);

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
        {showSidebar && (
          <>
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
            <li>
              <div className="theme-button">
                <div>
                  <Icon name={usingSystemTheme ? 'laptop' : darkMode ? 'moon outline' : 'sun outline'} size="large" />
                </div>
                <Dropdown>
                  <Dropdown.Toggle variant={darkMode ? 'dark' : 'light'} id="dropdown-basic">
                    Theme
                  </Dropdown.Toggle>
                  <Dropdown.Menu>
                    <Dropdown.Item
                      className={`${!usingSystemTheme && !darkMode ? 'active' : ''}`}
                      onSelect={() => setTheme('light')}
                    >
                      Light
                    </Dropdown.Item>
                    <Dropdown.Item
                      className={`${!usingSystemTheme && darkMode ? 'active' : ''}`}
                      onSelect={() => setTheme('dark')}
                    >
                      Dark
                    </Dropdown.Item>
                    <Dropdown.Item
                      className={`${usingSystemTheme ? 'active' : ''}`}
                      onSelect={() => setTheme('system')}
                    >
                      System
                    </Dropdown.Item>
                  </Dropdown.Menu>
                </Dropdown>
              </div>
            </li>
          </>
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
  } else {
    return (
      <div>
        <div className="sidebar">
          {/* Close Button */}
          <button className="sidebar-close" onClick={closeSidebar}>
            <XCircle className="sidebar-close-icon" size={'3vh'} />
          </button>

          {/* Profile Icon and Name */}
          <div className="sidebar-profile">
            <img src={picture ? picture : defaultAvatar} />
            <p>{name ? name : 'Anonymous Peter'}</p>
          </div>

          {/* Links */}
          {links}

          {/* Login/Logout */}
          <div className="sidebar-login">
            {isLoggedIn && (
              <a href={`/api/users/logout`}>
                <Button variant={darkMode ? 'dark' : 'light'}>
                  <span className="sidebar-login-icon">
                    <Icon name="sign out" className="sidebar-login-icon" />
                  </span>
                  Log Out
                </Button>
              </a>
            )}
            {!isLoggedIn && (
              <a href={`/api/users/auth/google`}>
                <Button variant={darkMode ? 'dark' : 'light'}>
                  <span className="sidebar-login-icon">
                    <Icon name="sign in" className="sidebar-login-icon" />
                  </span>
                  Log In
                </Button>
              </a>
            )}
          </div>
        </div>
      </div>
    );
  }
};

export default SideBar;
