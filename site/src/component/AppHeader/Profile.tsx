import { useCookies } from 'react-cookie';
import { useContext, useEffect, useState } from 'react';
import ThemeContext from '../../style/theme-context';
import { Button, OverlayTrigger, Popover } from 'react-bootstrap';
import { Icon } from 'semantic-ui-react';
import './Profile.scss';
import { NavLink } from 'react-router-dom';
import { ArrowLeftCircleFill } from 'react-bootstrap-icons';

type ProfileMenuTab = 'default' | 'theme';

const Profile = () => {
  const { darkMode, setTheme, usingSystemTheme } = useContext(ThemeContext);
  const [cookies] = useCookies(['user']);
  const [show, setShow] = useState(false);
  const [tab, setTab] = useState<ProfileMenuTab>('default');

  const { name, email, picture }: { name: string; email: string; picture: string } = cookies?.user ?? {};
  const isLoggedIn: boolean = cookies.user;

  useEffect(() => {
    if (!show) {
      setTimeout(() => setTab('default'), 150); // popover transition time is 150ms
    }
  }, [show]);

  const DefaultTab = (
    <>
      <div className="profile-popover__header">
        <img src={picture} alt={name} />
        <span>
          <h1 title={name}>{name}</h1>
          <h2 title={email}>{email}</h2>
        </span>
      </div>
      <div className="profile-popover__links">
        <ul>
          <li className="divider-before">
            <NavLink
              className={({ isActive }) => 'profile-popover__link' + (isActive ? ' active' : '')}
              to="/reviews"
              onClick={() => setShow(false)}
            >
              <div>
                <Icon name="sticky note outline" size="large" />
              </div>
              Your Reviews
            </NavLink>
          </li>
          <li>
            <button className="theme-button profile-popover__link" onClick={() => setTab('theme')}>
              <div>
                <Icon name={usingSystemTheme ? 'laptop' : darkMode ? 'moon outline' : 'sun outline'} size="large" />
              </div>
              Theme
            </button>
          </li>
          <li className="divider-before">
            <a href={`/api/users/logout`} className="profile-popover__link">
              <div>
                <Icon name="sign out" size="large" />
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
          <ArrowLeftCircleFill />
        </button>
      </div>
      <div className="profile-popover__links">
        <ul>
          <li className="divider-before">
            <button
              className={`theme-popover__link${!usingSystemTheme && !darkMode ? ' active' : ''}`}
              onClick={() => setTheme('light')}
            >
              <div>
                <Icon name="sun outline" size="large" />
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
                <Icon name="moon outline" size="large" />
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
                <Icon name="laptop" size="large" />
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
                    offset: [-150, 15],
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
              <Icon name="sign in" className="login-icon" />
            </span>
            Log In
          </Button>
        </a>
      )}
    </>
  );
};

export default Profile;
