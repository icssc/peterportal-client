import React, { useState, useEffect, Component, FC } from 'react';
import { Icon, Popup, Grid, Label, Header } from 'semantic-ui-react';
import { useLocation } from 'react-router-dom';

import { List } from 'react-bootstrap-icons';
import { ReactComponent as CogIcon } from '../../asset/cog.svg';
import { ReactComponent as ArrowIcon } from '../../asset/arrow.svg';
import { CSSTransition } from 'react-transition-group';
import { WeekData } from '../../types/types';

import Logo from '../../asset/peterportal-banner-logo.svg';
import './AppHeader.scss';

import { useAppDispatch } from '../../store/hooks';
import { setSidebarStatus } from '../../store/slices/uiSlice';

const AppHeader: FC<{}> = props => {
  const dispatch = useAppDispatch();
  const location = useLocation();
  const [week, setWeek] = useState('');

  let splitLocation = location.pathname.split('/');
  let coursesActive = splitLocation.length > 0 && splitLocation[splitLocation.length - 1] == 'courses';
  let professorsActive = splitLocation.length > 0 && splitLocation[splitLocation.length - 1] == 'professors';

  useEffect(() => {
    // Get the current week data
    fetch('/schedule/api/currentWeek')
      .then(res => res.json())
      .then((data: WeekData) => {
        // case for break and finals week
        if (data.week == -1) {
          setWeek(data.display);
        }
        // case when school is in session 
        else {
          setWeek('Week ' + data.week + ' • ' + data.quarter);
        }
      });
  }, [])

  let toggleMenu = () => {
    dispatch(setSidebarStatus(true));
  }

  return (
    <header className='navbar'>
      <div className='navbar-nav'>
        <div className='navbar-left'>
          {/* Hamburger Menu */}
          <div className='navbar-menu'>
            <List className='navbar-menu-icon' onClick={toggleMenu} />
          </div>

          {/* Toggle Course and Professor */}
          <div className='navbar-toggle'>
            <div className='desktop-toggle'>
              <div className={`navbar-toggle-item ${coursesActive ? 'active' : ''}`}>
                <a href='/search/courses'>
                  Courses
                </a>
              </div>
              <div className={`navbar-toggle-item ${professorsActive ? 'active' : ''}`}>
                <a href='/search/professors'>
                  Professors
                </a>
              </div>
            </div>
            <div className='mobile-toggle'>
              {coursesActive === true && (
                <div className={`navbar-toggle-item active`}>
                  <a href='/search/professors'>
                    Professors
                  </a>
                </div>
              )}
              {professorsActive === true && (
                <div className={`navbar-toggle-item active`}>
                  <a href='/search/courses'>
                    Courses
                  </a>
                </div>
              )}
            </div>
          </div>
        </div>

        {/* Logo */}
        <div className='navbar-logo'>
          <a href='/'>
            <img alt='PeterPortal' id='peterportal-logo' src={Logo} height='100%'></img>
          </a>
        </div>

        {/* Week */}
        <div style={{ display: 'flex' }}>
          <p className='school-term' style={{ marginBottom: '-1px' }}>
            {week}
          </p>
        </div>
      </div>
    </header>
  );
}

export default AppHeader;