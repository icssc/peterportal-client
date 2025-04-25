import { useState, useEffect, FC } from 'react';
import { Link } from 'react-router-dom';

import { ChatLeftDotsFill, Github, List } from 'react-bootstrap-icons';

import Logo from '../../asset/peterportal-banner-logo.svg';
import './AppHeader.scss';

import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setSidebarStatus } from '../../store/slices/uiSlice';
import Profile from './Profile';
import trpc from '../../trpc';
import { Popover } from 'react-bootstrap';
import PPCOverlayTrigger from '../PPCOverlayTrigger';

const AppHeader: FC = () => {
  const dispatch = useAppDispatch();
  const sidebarOpen = useAppSelector((state) => state.ui.sidebarOpen);
  const [week, setWeek] = useState('');

  useEffect(() => {
    // Get the current week data
    trpc.schedule.currentWeek.query().then((res) => {
      /** @todo make this less code-smelly */
      setWeek(res.display.split(' • ')[0]);
    });
  }, []);

  const toggleMenu = () => {
    dispatch(setSidebarStatus(!sidebarOpen));
  };

  const popover = (
    <Popover.Content>
      <h4>Beta Disclaimer</h4>
      <p>
        Please note that this is a beta version of PeterPortal, which is still undergoing development. Some content on
        this web application may not be accurate. Users are encouraged to double check details.
      </p>
      <p>
        Should you encounter any bugs, glitches, lack of functionality or other problems on the application, please let
        us know immediately so we can rectify these accordingly. Your help in this regard is greatly appreciated.
      </p>
      <div className="feedback">
        <a
          className="ui button"
          href="https://github.com/icssc-projects/peterportal-client/issues/new"
          target="_blank"
          rel="noopener noreferrer"
        >
          <Github /> Report an issue
        </a>
        <a
          className="ui button"
          href="https://form.asana.com/?k=4h9ZTRkVUT9ZwfJrmvxDDw&d=1208267282546207"
          target="_blank"
          rel="noopener noreferrer"
        >
          <ChatLeftDotsFill /> Feedback
        </a>
      </div>
    </Popover.Content>
  );

  return (
    <header className="navbar">
      <div className="navbar-nav">
        <div className="navbar-left">
          {/* Hamburger Menu */}
          <button className="navbar-menu" onClick={toggleMenu}>
            <List className="navbar-menu-icon" size="32px" />
          </button>
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
            <PPCOverlayTrigger popoverContent={popover} placement="bottom">
              <div
                style={{
                  display: 'inline-flex',
                  alignItems: 'center',
                  backgroundColor: '#fbbd08', // Yellow background
                  color: 'white',
                  padding: '.2em 1em',
                  borderRadius: '4px',
                  fontSize: '0.9em',
                  fontWeight: 'bold',
                  textDecoration: 'none',
                  cursor: 'pointer',
                }}
              >
                beta
                <span
                  style={{
                    marginLeft: '0.5em',
                    backgroundColor: 'rgba(0, 0, 0, 0.1)',
                    padding: '0.2em 0.5em',
                    borderRadius: '3px',
                  }}
                >
                  v1.2
                </span>
              </div>
            </PPCOverlayTrigger>
          </div>
          <p className="school-term" style={{ height: '1rem', lineHeight: '1rem' }}>
            {week}
          </p>
          <Profile />
        </div>
      </div>
    </header>
  );
};

export default AppHeader;
