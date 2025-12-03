import { useContext, useState } from 'react';
import ThemeContext from '../../style/theme-context';

import { Button, List, ListItem, ListItemButton, ListItemIcon, ListItemText, Popover } from '@mui/material';
import './Profile.scss';

import Link from 'next/link';
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
import { Theme } from '@peterportal/types';

interface AdminProfileLinksProps {
  pathname: string | null;
  onClose: () => void;
}

const AdminProfileLinks = ({ pathname, onClose }: AdminProfileLinksProps) => {
  return (
    <>
      <ListItem>
        <ListItemButton
          className={'profile-popover__link' + (pathname === '/admin/verify' ? ' active' : '')}
          href="/admin/verify"
          onClick={onClose}
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
          onClick={onClose}
          component={Link}
        >
          <ListItemIcon>
            <FlagIcon />
          </ListItemIcon>
          <ListItemText primary="View Reports" />
        </ListItemButton>
      </ListItem>
    </>
  );
};

const Profile = () => {
  const { darkMode, setTheme, usingSystemTheme } = useContext(ThemeContext);
  const pathname = usePathname();
  const [anchorEl, setAnchorEl] = useState<HTMLElement | null>(null);
  const open = Boolean(anchorEl);

  const handleClick = (event: React.MouseEvent<HTMLElement>) => {
    if (open) {
      setAnchorEl(null);
    } else {
      setAnchorEl(event.currentTarget);
    }
  };

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

  const getCurrentTheme = (): Theme => {
    if (usingSystemTheme) return 'system';
    return darkMode ? 'dark' : 'light';
  };

  const handleThemeChange = (tab: string) => {
    setTheme(tab as Theme);
  };

  const popover = (
    <div className="ppc-popover-inner">
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
            onClick={() => setAnchorEl(null)}
            component={Link}
          >
            <ListItemIcon>
              <StickyNote2OutlinedIcon />
            </ListItemIcon>
            <ListItemText primary="Your Reviews" />
          </ListItemButton>
        </ListItem>
        {isAdmin && <AdminProfileLinks pathname={pathname} onClose={() => setAnchorEl(null)} />}
        <ListItem>
          <ListItemButton href={'/api/users/auth/logout'} className="profile-popover__link" component="a">
            <ListItemIcon>
              <LogoutIcon />
            </ListItemIcon>
            <ListItemText primary="Log Out" />
          </ListItemButton>
        </ListItem>
      </List>
    </div>
  );

  return (
    <div className="navbar-profile">
      <button className="profile-button" onClick={handleClick}>
        <Image src={picture} alt={name} className="navbar-profile-pic" width={36} height={36} />
      </button>
      <Popover
        open={open}
        anchorEl={anchorEl}
        onClose={() => setAnchorEl(null)}
        anchorOrigin={{
          vertical: 'bottom',
          horizontal: 'right',
        }}
        transformOrigin={{
          vertical: 'top',
          horizontal: 'right',
        }}
        slotProps={{
          paper: {
            className: 'ppc-popover-paper',
          },
        }}
      >
        {popover}
      </Popover>
    </div>
  );
};

export default Profile;
