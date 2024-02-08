import { useState, useEffect, FC } from 'react';
import { Icon, Popup, Grid, Label } from 'semantic-ui-react';
import { useLocation, Link } from 'react-router-dom';
import axios from 'axios';

import { List } from 'react-bootstrap-icons';
import { WeekData } from '../../types/types';

import Logo from '../../asset/peterportal-banner-logo.svg';
import './AppHeader.scss';

import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setSidebarStatus } from '../../store/slices/uiSlice';
import Profile from './Profile';
import { isDesktop, isMobile } from 'react-device-detect';

const AppHeader: FC = () => {
  const dispatch = useAppDispatch();
  const sidebarOpen = useAppSelector((state) => state.ui.sidebarOpen);
  const location = useLocation();
  const [week, setWeek] = useState('');

  const splitLocation = location.pathname.split('/');
  const coursesActive =
    splitLocation[1] === '' ||
    (splitLocation.length > 0 && splitLocation[splitLocation.length - 1] == 'courses') ||
    (splitLocation.length > 1 && splitLocation[1] == 'course');
  const professorsActive =
    (splitLocation.length > 0 && splitLocation[splitLocation.length - 1] == 'professors') ||
    (splitLocation.length > 1 && splitLocation[1] == 'professor');

  useEffect(() => {
    // Get the current week data
    axios.get<WeekData>('/api/schedule/api/currentWeek').then((res) => {
      setWeek(res.data.display);
    });
  }, []);

  const toggleMenu = () => {
    dispatch(setSidebarStatus(!sidebarOpen));
  };

  return (
    <header className="navbar">
      <div className="navbar-nav">
        <div className="navbar-left">
          {/* Hamburger Menu */}
          {isMobile && (
            <div className="navbar-menu">
              <List className="navbar-menu-icon" onClick={toggleMenu} />
            </div>
          )}

          {/* Toggle Course and Professor */}
          {(coursesActive || professorsActive) && (
            <div className="navbar-toggle">
              <div className="desktop-toggle">
                <div className={`navbar-toggle-item ${coursesActive ? 'active' : ''}`}>
                  <Link to={'/search/courses'}>Courses</Link>
                </div>
                <div className={`navbar-toggle-item ${professorsActive ? 'active' : ''}`}>
                  <Link to={'/search/professors'}>Professors</Link>
                </div>
              </div>
              <div className="mobile-toggle">
                {coursesActive === true && (
                  <div className={`navbar-toggle-item active`}>
                    <Link to={'/search/professors'}>Professors</Link>
                  </div>
                )}
                {professorsActive === true && (
                  <div className={`navbar-toggle-item active`}>
                    <Link to={'/search/courses'}>Courses</Link>
                  </div>
                )}
              </div>
            </div>
          )}
        </div>

        {/* Logo */}
        <div className="navbar-logo">
          <Link to={'/'}>
            <img alt="PeterPortal" id="peterportal-logo" src={Logo}></img>
          </Link>
        </div>

        {/* Week */}
        <div style={{ display: 'flex', alignItems: 'center', height: '100%' }}>
          <div className="beta" style={{ margin: 'auto 12px' }}>
            <Popup
              className="beta-popup"
              style={{ padding: '36px', width: '400px' }}
              position="bottom right"
              trigger={
                <Label as="a" color="yellow" image>
                  beta<Label.Detail>v1.1</Label.Detail>
                </Label>
              }
              flowing
              hoverable
            >
              <Grid centered columns={1}>
                <Grid.Column textAlign="left">
                  <h4>Beta Disclaimer</h4>
                  <p>
                    Please note that this is a beta version of PeterPortal, which is still undergoing development. Some
                    content on this web application may not be accurate. Users are encouraged to double check details.
                    <br />
                    <br />
                    Should you encounter any bugs, glitches, lack of functionality or other problems on the application,
                    please let us know immediately so we can rectify these accordingly. Your help in this regard is
                    greatly appreciated.
                  </p>
                  <div className="feedback">
                    <a
                      className="ui button"
                      href="https://github.com/icssc-projects/peterportal-client/issues/new"
                      target="_blank"
                      rel="noopener noreferrer"
                    >
                      <Icon name="github" />
                      Report an issue
                    </a>
                    <a
                      className="ui button"
                      href="https://forms.gle/JjwBmELq26daroTh9"
                      target="_blank"
                      rel="noopener noreferrer"
                    >
                      <Icon name="clipboard list" />
                      Feedback
                    </a>
                  </div>
                </Grid.Column>
              </Grid>
            </Popup>
          </div>
          <p className="school-term" style={{ height: '1rem', lineHeight: '1rem' }}>
            {week}
          </p>
          {isDesktop && <Profile />}
        </div>
      </div>
    </header>
  );
};

export default AppHeader;
