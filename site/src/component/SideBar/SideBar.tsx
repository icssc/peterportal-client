import React, { useContext, useEffect, useState } from 'react';
import { NavLink } from 'react-router-dom';
import { Icon } from 'semantic-ui-react';
import { XCircle } from 'react-bootstrap-icons';
import { useCookies } from 'react-cookie';
import './Sidebar.scss'
import DefaultAvatar from '../../asset/default-avatar.png';
import { Button } from 'react-bootstrap';

import { useAppSelector, useAppDispatch } from '../..//store/hooks';
import { setSidebarStatus } from '../../store/slices/uiSlice';
import axios, { AxiosResponse } from 'axios';
import ThemeContext from 'src/style/theme-context';

const SideBar = () => {
  const dispatch = useAppDispatch();
  const showSidebar = useAppSelector(state => state.ui.sidebarOpen);
  const [cookies, setCookie] = useCookies(['user']);
  const [name, setName] = useState('');
  const [picture, setPicture] = useState('');
  const [isAdmin, setIsAdmin] = useState<Boolean>(false);
  const { darkMode, toggleTheme } = useContext(ThemeContext);

  let isLoggedIn = cookies.hasOwnProperty('user');

  interface AdminResponse {
    admin: boolean
  }

  const checkAdmin = async () => {
    const res: AxiosResponse<AdminResponse> = await axios.get('/api/users/isAdmin');
    const admin = res.data.admin;
    setIsAdmin(admin);
  }

  useEffect(() => {
    // if the user is logged in
    if (isLoggedIn) {
      setName(cookies.user.name);
      setPicture(cookies.user.picture);
    }
    checkAdmin();
  })

  const closeSidebar = () => dispatch(setSidebarStatus(false));

  let links = <div className='sidebar-links'>
    <ul>
      <li>
        <NavLink to='/' activeClassName='sidebar-active' onClick={closeSidebar} isActive={(match, location) => {
          let splitLocation = location.pathname.split('/');
          return splitLocation.length > 1 && ['search', 'course', 'professor'].includes(splitLocation[1]);
        }}>
          <div>
            <Icon name='list alternate outline' size='large' />
          </div>
          <span>
            Catalogue
          </span>
        </NavLink>
      </li>
      <li>
        <NavLink to='/roadmap' activeClassName='sidebar-active' onClick={closeSidebar}>
          <div>
            <Icon name='map outline' size='large' />
          </div>
          <span>
            Peter's Roadmap
          </span>
        </NavLink>
      </li>
      {showSidebar && <li>
        {/* eslint-disable-next-line jsx-a11y/anchor-is-valid */}
        <a className='theme-toggle' onClick={toggleTheme}>
          <div>
            <Icon name={darkMode ? 'moon outline' : 'sun outline'} size='large' />
          </div>
          <span>
            Toggle Dark Mode
          </span>
        </a>
      </li>}
      {isAdmin && <>
      <li>
        <NavLink to='/admin/verify' activeClassName='sidebar-active' onClick={closeSidebar}>
          <div>
            <Icon name='check' size='large' />
          </div>
          <span>
            Verify Reviews
          </span>
        </NavLink>
      </li>
      <li>
        <NavLink to='/admin/reports' activeClassName='sidebar-active' onClick={closeSidebar}>
          <div>
            <Icon name='exclamation triangle' size='large' />
          </div>
          <span>
            View Reports
          </span>
        </NavLink>
      </li> </>
      }
    </ul>
  </div>


  if (!showSidebar) {
    return <div className='sidebar mini'>
      {links}
    </div>
  }
  return (
    <div className='sidebar'>
      {/* Close Button */}
      <div className='sidebar-close'>
        <XCircle className='sidebar-close-icon' onClick={closeSidebar} />
      </div>

      {/* Profile Icon and Name */}
      <div className='sidebar-profile'>
        <img src={picture ? picture : DefaultAvatar} />
        <p>
          {name ? name : 'Anonymous Peter'}
        </p>
      </div>

      {/* Links */}
      {links}

      {/* Login/Logout */}
      <div className='sidebar-login'>
        {isLoggedIn && <a href={`/api/users/logout`}>
          <Button variant={darkMode ? 'dark' : 'light'}>
            <span className='sidebar-login-icon'>
              <Icon name='sign out' className='sidebar-login-icon' />
            </span>
            Log Out
          </Button>
        </a>}
        {!isLoggedIn && <a href={`/api/users/auth/google`}>
          <Button variant={darkMode ? 'dark' : 'light'}>
            <span className='sidebar-login-icon'>
              <Icon name='sign in' className='sidebar-login-icon' />
            </span>
            Log In
          </Button>
        </a>}
      </div>
    </div>
  )
}

export default SideBar;
