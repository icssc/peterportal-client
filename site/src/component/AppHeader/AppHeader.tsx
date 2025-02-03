import { useState, useEffect, FC } from 'react';
import { Grid, Label } from 'semantic-ui-react';
import { Link } from 'react-router-dom';

import { ChatLeftDotsFill, Github, List } from 'react-bootstrap-icons';

import Logo from '../../asset/peterportal-banner-logo.svg';
import './AppHeader.scss';

import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setSidebarStatus } from '../../store/slices/uiSlice';
import Profile from './Profile';
import trpc from '../../trpc';
import { OverlayTrigger, Popover } from 'react-bootstrap';

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
    <Popover className="beta-popup ppc-popover">
      <Popover.Content>
        <Grid centered columns={1}>
          <Grid.Column textAlign="left">
            <h4>Beta Disclaimer</h4>
            <p>
              Please note that this is a beta version of PeterPortal, which is still undergoing development. Some
              content on this web application may not be accurate. Users are encouraged to double check details.
            </p>
            <p>
              Should you encounter any bugs, glitches, lack of functionality or other problems on the application,
              please let us know immediately so we can rectify these accordingly. Your help in this regard is greatly
              appreciated.
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
          </Grid.Column>
        </Grid>
      </Popover.Content>
    </Popover>
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
            <OverlayTrigger overlay={popover} placement="bottom">
              <Label as="a" color="yellow" image>
                beta<Label.Detail>v1.1</Label.Detail>
              </Label>
            </OverlayTrigger>
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
