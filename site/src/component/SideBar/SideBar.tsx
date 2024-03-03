import { useEffect, useState } from 'react';
import { NavLink } from 'react-router-dom';
import { Icon } from 'semantic-ui-react';
import { XCircle } from 'react-bootstrap-icons';
import { useCookies } from 'react-cookie';
import './Sidebar.scss';
import { CSSTransition } from 'react-transition-group';

import { useAppSelector, useAppDispatch } from '../..//store/hooks';
import { setSidebarStatus } from '../../store/slices/uiSlice';
import axios, { AxiosResponse } from 'axios';
import Footer from '../Footer/Footer';

const SideBar = () => {
  const dispatch = useAppDispatch();
  const showSidebar = useAppSelector((state) => state.ui.sidebarOpen);
  const [cookies] = useCookies(['user']);
  const [isAdmin, setIsAdmin] = useState<boolean>(false);

  const isLoggedIn = cookies.user !== undefined;

  interface AdminResponse {
    admin: boolean;
  }

  useEffect(() => {
    // if the user is logged in
    if (isLoggedIn) {
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
            className={({ isActive }) => (isActive || location.pathname === '/search/courses' ? 'sidebar-active' : '')}
            onClick={closeSidebar}
          >
            <div>
              <Icon name="list alternate outline" size="large" />
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
              <Icon name="users" size="large" />
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
              <Icon name="map outline" size="large" />
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
                  <Icon name="check" size="large" />
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
                  <Icon name="exclamation triangle" size="large" />
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
        {/* Clicking this is only an alternative action to something that is already accessible */}
        {/* eslint-disable-next-line jsx-a11y/no-static-element-interactions,jsx-a11y/click-events-have-key-events */}
        <div className="sidebar-overlay" onClick={closeSidebar}></div>
      </CSSTransition>
      <CSSTransition in={showSidebar} timeout={500} unmountOnExit>
        <div className="sidebar">
          {/* Close Button */}
          <button className="sidebar-close" onClick={closeSidebar} id="close-sidebar-btn">
            <XCircle className="sidebar-close-icon" size={'3vh'} />
          </button>

          {links}

          <Footer />
        </div>
      </CSSTransition>
    </>
  );
};

export default SideBar;
