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
  let coursesActive = splitLocation.length > 0 && splitLocation[splitLocation.length - 1] == 'courses' || splitLocation.length > 1 && splitLocation[1] == 'course';
  let professorsActive = splitLocation.length > 0 && splitLocation[splitLocation.length - 1] == 'professors' || splitLocation.length > 1 && splitLocation[1] == 'professor';

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
          {(coursesActive || professorsActive) && <div className='navbar-toggle'>
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
          }
        </div>

        {/* Logo */}
        <div className='navbar-logo'>
          <a href='/'>
            <img alt='PeterPortal' id='peterportal-logo' src={Logo}></img>
          </a>
        </div>

        {/* Week */}
        <div style={{ display: 'flex', alignItems: 'center' }}>
          <div className='beta' style={{ margin: "auto 12px" }}>
            <Popup style={{ padding: "36px", width: "400px" }} position='bottom right' trigger={<Label as='a' color='yellow' image>beta<Label.Detail>v1.1</Label.Detail></Label>} flowing hoverable >
              <Grid centered columns={1}>
                <Grid.Column textAlign='left'>
                  <h4>Beta Disclaimer</h4>
                  <p>
                    Please note that this is a beta version of PeterPortal, which is still undergoing development.
                    Some content on this web application may not be accurate. Users are encouraged to double check details.
                    <br />
                    <br />
                    Should you encounter any bugs, glitches, lack of functionality or other problems on the application,
                    please let us know immediately so we can rectify these accordingly. Your help in this regard is greatly appreciated.
                  </p>
                  <a className="ui button" href="https://github.com/icssc-projects/peterportal-client/issues/new" target="_blank" rel="noopener noreferrer">
                    <Icon name='github' />Report an issue
                  </a>
                </Grid.Column>
              </Grid>
            </Popup>
          </div>
          <p className='school-term' style={{ height: '1rem', lineHeight: '1rem' }}>
            {week}
          </p>
        </div>
      </div>
    </header>
  );
}

export default AppHeader;