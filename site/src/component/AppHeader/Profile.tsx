import { useContext, useEffect, useState } from 'react';
import ThemeContext from '../../style/theme-context';
import { Button, OverlayTrigger, Popover } from 'react-bootstrap';
import './Profile.scss';
import { NavLink } from 'react-router-dom';
import { ArrowLeftCircleFill, BoxArrowInRight, BoxArrowRight, Laptop, Moon, Sticky, Sun } from 'react-bootstrap-icons';
import trpc from '../../trpc';
import { useIsLoggedIn } from '../../hooks/isLoggedIn';

type ProfileMenuTab = 'default' | 'theme';

const Profile = () => {
  const { darkMode, setTheme, usingSystemTheme } = useContext(ThemeContext);
  const [show, setShow] = useState(false);
  const [tab, setTab] = useState<ProfileMenuTab>('default');

  const [name, setName] = useState('');
  const [email, setEmail] = useState('');
  const [picture, setPicture] = useState('');
  const isLoggedIn = useIsLoggedIn();

  useEffect(() => {
    if (!show) {
      setTimeout(() => setTab('default'), 150); // popover transition time is 150ms
    }
  }, [show]);

  useEffect(() => {
    if (isLoggedIn) {
      trpc.users.get.query().then((user) => {
        setName(user.name);
        setEmail(user.email);
        setPicture(user.picture);
      });
    }
  });

  const DefaultTab = (
    <>
      <div className="profile-popover__header">
        <img src={picture} alt={name} width="50" height="50" />
        <div>
          <h1 title={name}>{name}</h1>
          <h2 title={email}>{email}</h2>
        </div>
      </div>
      <div className="profile-popover__links">
        <ul>
          <li>
            <NavLink
              className={({ isActive }) => 'profile-popover__link' + (isActive ? ' active' : '')}
              to="/reviews"
              onClick={() => setShow(false)}
            >
              <div>
                <Sticky />
              </div>
              Your Reviews
            </NavLink>
          </li>
          <li>
            <button className="theme-button profile-popover__link" onClick={() => setTab('theme')}>
              <div>{usingSystemTheme ? <Laptop /> : darkMode ? <Moon /> : <Sun />}</div>
              Theme
            </button>
          </li>
          <li>
            <a href={'/api/users/auth/logout'} className="profile-popover__link">
              <div>
                <BoxArrowRight />
              </div>
              Log Out
            </a>
          </li>
        </ul>
      </div>
    </>
  );

  const ThemeTab = (
    <>
      <div className="profile-popover__header">
        <button onClick={() => setTab('default')}>
          <ArrowLeftCircleFill /> <span>Back</span>
        </button>
      </div>
      <div className="profile-popover__links">
        <ul>
          <li>
            <button
              className={`theme-popover__link${!usingSystemTheme && !darkMode ? ' active' : ''}`}
              onClick={() => setTheme('light')}
            >
              <div>
                <Sun />
              </div>
              Light
            </button>
          </li>
          <li>
            <button
              className={`theme-popover__link${!usingSystemTheme && darkMode ? ' active' : ''}`}
              onClick={() => setTheme('dark')}
            >
              <div>
                <Moon />
              </div>
              Dark
            </button>
          </li>
          <li>
            <button
              className={`theme-popover__link${usingSystemTheme ? ' active' : ''}`}
              onClick={() => setTheme('system')}
            >
              <div>
                <Laptop />
              </div>
              System
            </button>
          </li>
        </ul>
      </div>
    </>
  );

  const ProfilePopover = (
    <Popover id="profile-popover">
      {tab === 'default' && DefaultTab}
      {tab === 'theme' && ThemeTab}
    </Popover>
  );

  return (
    <>
      {isLoggedIn ? (
        <div className="navbar-profile">
          <OverlayTrigger
            rootClose
            trigger="click"
            placement="bottom"
            overlay={ProfilePopover}
            show={show}
            onToggle={setShow}
            popperConfig={{
              modifiers: [
                {
                  name: 'offset',
                  options: {
                    offset: [-135, 8],
                  },
                },
              ],
            }}
          >
            {({ ref, ...triggerHandler }) => (
              <button {...triggerHandler} className="profile-button">
                <img ref={ref} src={picture} alt={name} className="navbar-profile-pic" />
              </button>
            )}
          </OverlayTrigger>
        </div>
      ) : (
        <a href={`/api/users/auth/google`}>
          <Button variant={darkMode ? 'dark' : 'light'}>
            <span>
              <BoxArrowInRight />{' '}
            </span>
            Log In
          </Button>
        </a>
      )}
    </>
  );
};

export default Profile;
