import { useContext, useState } from 'react';
import ThemeContext from '../../style/theme-context';
import { OverlayTrigger, Popover } from 'react-bootstrap';
import { Button, List, ListItem, ListItemButton, ListItemIcon, ListItemText } from '@mui/material';
import './Profile.scss';

import Link from '@mui/material/Link';
import ExitToAppIcon from '@mui/icons-material/ExitToApp';
import LogoutIcon from '@mui/icons-material/Logout';
import SettingsBrightnessIcon from '@mui/icons-material/SettingsBrightness';
import LightModeIcon from '@mui/icons-material/LightMode';
import DarkModeIcon from '@mui/icons-material/DarkMode';
import StickyNote2OutlinedIcon from '@mui/icons-material/StickyNote2Outlined';
import GradingIcon from '@mui/icons-material/Grading';
import FlagIcon from '@mui/icons-material/Flag';
import Divider from '@mui/material/Divider';
import { usePathname } from 'next/navigation';
import { useAppSelector } from '../../store/hooks';
import Image from 'next/image';
import TabSelector, { TabOption } from '../../app/roadmap/sidebar/TabSelector';

const Profile = () => {
  const { darkMode, setTheme, usingSystemTheme } = useContext(ThemeContext);
  const [show, setShow] = useState(false);
  const pathname = usePathname();

  const user = useAppSelector((state) => state.user.user);
  const isAdmin = useAppSelector((state) => state.user.isAdmin);

  if (!user) {
    return (
      <a href={`/api/users/auth/google`} className="login-button">
        <Button startIcon={<ExitToAppIcon />} color="inherit">
          Log In
        </Button>
      </a>
    );
  }

  const { name, email, picture } = user;

  const themeTabs: TabOption[] = [
    { value: 'light', label: 'Light', icon: <LightModeIcon /> },
    { value: 'system', label: 'System', icon: <SettingsBrightnessIcon /> },
    { value: 'dark', label: 'Dark', icon: <DarkModeIcon /> },
  ];

  const getCurrentTheme = (): string => {
    if (usingSystemTheme) return 'system';
    return darkMode ? 'dark' : 'light';
  };

  const handleThemeChange = (tab: string) => {
    setTheme(tab as 'light' | 'system' | 'dark');
  };

  const ProfilePopover = (
    <Popover id="profile-popover" className="ppc-popover">
      <div className="popover-body">
        <div className="profile-popover__header">
          <Image src={picture} alt={name} width="50" height="50" />
          <div>
            <h1 title={name}>{name}</h1>
            <h2 title={email}>{email}</h2>
          </div>
        </div>
        <div className="profile-popover__theme-section">
          <h4>Theme</h4>
          <TabSelector tabs={themeTabs} selectedTab={getCurrentTheme()} onTabChange={handleThemeChange} />
          <Divider />
        </div>
        <List className="profile-popover__links">
          <ListItem>
            <ListItemButton
              className={'profile-popover__link' + (pathname === '/reviews' ? ' active' : '')}
              href="/reviews"
              onClick={() => setShow(false)}
              component={Link}
            >
              <ListItemIcon>
                <StickyNote2OutlinedIcon />
              </ListItemIcon>
              <ListItemText primary="Your Reviews" />
            </ListItemButton>
          </ListItem>
          {isAdmin && (
            <>
              <ListItem>
                <ListItemButton
                  className={'profile-popover__link' + (pathname === '/admin/verify' ? ' active' : '')}
                  href="/admin/verify"
                  onClick={() => setShow(false)}
                  component={Link}
                >
                  <ListItemIcon>
                    <GradingIcon />
                  </ListItemIcon>
                  <ListItemText primary="Verify Reviews" />
                </ListItemButton>
              </ListItem>
              <ListItem>
                <ListItemButton
                  className={'profile-popover__link' + (pathname === '/admin/reports' ? ' active' : '')}
                  href="/admin/reports"
                  onClick={() => setShow(false)}
                  component={Link}
                >
                  <ListItemIcon>
                    <FlagIcon />
                  </ListItemIcon>
                  <ListItemText primary="View Reports" />
                </ListItemButton>
              </ListItem>
            </>
          )}
          <ListItem>
            <ListItemButton href={'/api/users/auth/logout'} className="profile-popover__link" component={Link}>
              <ListItemIcon>
                <LogoutIcon />
              </ListItemIcon>
              <ListItemText primary="Log Out" />
            </ListItemButton>
          </ListItem>
        </List>
      </div>
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
            <Image ref={ref} src={picture} alt={name} className="navbar-profile-pic" width={36} height={36} />
          </button>
        )}
      </OverlayTrigger>
    </div>
  );
};

export default Profile;
