import { useContext, useEffect, useState } from 'react';
import ThemeContext from '../../style/theme-context';
import { Button, OverlayTrigger, Popover } from 'react-bootstrap';
import './Profile.scss';

import ArrowCircleLeftIcon from '@mui/icons-material/ArrowCircleLeft';
import ExitToAppIcon from '@mui/icons-material/ExitToApp';
import LogoutIcon from '@mui/icons-material/Logout';
import LaptopIcon from '@mui/icons-material/Laptop';
import LightModeIcon from '@mui/icons-material/LightMode';
import DarkModeIcon from '@mui/icons-material/DarkMode';
import StickyNote2OutlinedIcon from '@mui/icons-material/StickyNote2Outlined';
import Link from 'next/link';
import { usePathname } from 'next/navigation';
import { useAppSelector } from '../../store/hooks';

type ProfileMenuTab = 'default' | 'theme';

const Profile = () => {
  const { darkMode, setTheme, usingSystemTheme } = useContext(ThemeContext);
  const [show, setShow] = useState(false);
  const [tab, setTab] = useState<ProfileMenuTab>('default');
  const pathname = usePathname();

  // const [name, setName] = useState('');
  // const [email, setEmail] = useState('');
  // const [picture, setPicture] = useState<string | undefined>(undefined);
  // const isLoggedIn = useIsLoggedIn();

  const user = useAppSelector((state) => state.user.user);

  useEffect(() => {
    if (!show) {
      setTimeout(() => setTab('default'), 150); // popover transition time is 150ms
    }
  }, [show]);

  useEffect(() => {
    // if (isLoggedIn) {
    //   trpc.users.get.query().then((user) => {
    //     setName(user.name);
    //     setEmail(user.email);
    //     setPicture(user.picture);
    //   });
    // }
  });

  if (!user) {
    return (
      <a href={`/api/users/auth/google`}>
        <Button variant={darkMode ? 'dark' : 'light'}>
          <span>
            <ExitToAppIcon className="exit-icon" />
          </span>
          Log In
        </Button>
      </a>
    );
  }

  const { name, email, picture } = user;

  /** @todo change to <Image/> once we get user data to be server-rendered */
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
            <Link
              className={'profile-popover__link' + (pathname === '/reviews' ? ' active' : '')}
              href="/reviews"
              onClick={() => setShow(false)}
            >
              <div>
                <StickyNote2OutlinedIcon />
              </div>
              Your Reviews
            </Link>
          </li>
          <li>
            <button className="theme-button profile-popover__link" onClick={() => setTab('theme')}>
              <div>{usingSystemTheme ? <LaptopIcon /> : darkMode ? <DarkModeIcon /> : <LightModeIcon />}</div>
              Theme
            </button>
          </li>
          <li>
            <a href={'/api/users/auth/logout'} className="profile-popover__link">
              <div>
                <LogoutIcon />
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
          <ArrowCircleLeftIcon /> <span>Back</span>
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
                <LightModeIcon />
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
                <DarkModeIcon />
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
                <LaptopIcon />
              </div>
              System
            </button>
          </li>
        </ul>
      </div>
    </>
  );

  const ProfilePopover = (
    <Popover id="profile-popover" className="ppc-popover">
      {tab === 'default' ? DefaultTab : ThemeTab}
    </Popover>
  );

  return (
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
  );
};

export default Profile;
