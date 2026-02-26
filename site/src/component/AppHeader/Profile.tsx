import React, { FC, useContext, useState } from 'react';
import ThemeContext from '../../style/theme-context';

import { List, ListItem, ListItemButton, ListItemIcon, ListItemText, Popover, Divider } from '@mui/material';
import './Profile.scss';

import Link from 'next/link';

import EventNoteIcon from '@mui/icons-material/EventNote';
import AccountCircleIcon from '@mui/icons-material/AccountCircle';
import LogoutIcon from '@mui/icons-material/Logout';
import SettingsBrightnessIcon from '@mui/icons-material/SettingsBrightness';
import LightModeIcon from '@mui/icons-material/LightMode';
import DarkModeIcon from '@mui/icons-material/DarkMode';
import RateReviewIcon from '@mui/icons-material/RateReview';
import PlaylistAddCheckIcon from '@mui/icons-material/PlaylistAddCheck';
import FlagIcon from '@mui/icons-material/Flag';
import FavoriteRoundedIcon from '@mui/icons-material/FavoriteRounded';
// import InfoIcon from '@mui/icons-material/Info';
import AssignmentIcon from '@mui/icons-material/Assignment';

import { usePathname } from 'next/navigation';
import { useAppSelector } from '../../store/hooks';
import Image from 'next/image';
import TabSelector, { TabOption } from '../../app/roadmap/sidebar/TabSelector';
import { Theme, UserMetadata } from '@peterportal/types';
import { useIsMobile } from '../../helpers/util';
import { useIsLoggedIn } from '../../hooks/isLoggedIn';
import ProfileMenuButtons from '../../shared-components/ProfileMenuButtons';

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
            <PlaylistAddCheckIcon />
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

interface ProfileMenuLinksProps {
  handleLinkClick: () => void;
}
const ProfileMenuLinks: FC<ProfileMenuLinksProps> = ({ handleLinkClick }) => {
  const pathname = usePathname();
  const isAdmin = useAppSelector((state) => state.user.isAdmin);
  const isMobile = useIsMobile();
  const isLoggedIn = useIsLoggedIn();

  return (
    <List className="profile-popover-links">
      {isMobile && (
        <ListItem>
          <ListItemButton href={'https://antalmanac.com'} className="profile-popover-link" component="a">
            <ListItemIcon>
              <EventNoteIcon />
            </ListItemIcon>
            <ListItemText primary="Go to Scheduler" />
          </ListItemButton>
        </ListItem>
      )}
      {isLoggedIn && (
        <ListItem>
          <ListItemButton
            className={'profile-popover-link' + (pathname === '/reviews' ? ' active' : '')}
            href="/reviews"
            onClick={handleLinkClick}
            component={Link}
          >
            <ListItemIcon>
              <RateReviewIcon />
            </ListItemIcon>
            <ListItemText primary="Your Reviews" />
          </ListItemButton>
        </ListItem>
      )}
      {isLoggedIn && isAdmin && <AdminProfileLinks pathname={pathname} onClose={handleLinkClick} />}
    </List>
  );
};

const UserInformation: FC<{ user: UserMetadata | null }> = ({ user }) => {
  if (!user) return null;

  const { name, email, picture } = user;

  return (
    <div className="profile-popover-header">
      <Image src={picture} alt={name} width="50" height="50" />
      <div>
        <h1 title={name}>{name}</h1>
        <h2 title={email}>{email}</h2>
      </div>
    </div>
  );
};

const ProfileThemeMenu = () => {
  const { darkMode, setTheme, usingSystemTheme } = useContext(ThemeContext);

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

  return (
    <div className="profile-popover-theme-selector">
      <h4>Theme</h4>
      <TabSelector tabs={themeTabs} selectedTab={getCurrentTheme()} onTabChange={handleThemeChange} />
    </div>
  );
};

const DonateAboutFeedbackButtons = () => {
  return (
    <List className="profile-popover-links">
      <ListItem>
        <ListItemButton
          href={'https://venmo.com/u/ICSSC'}
          className="profile-popover-link"
          component="a"
          target="_blank"
        >
          <ListItemIcon>
            <FavoriteRoundedIcon />
          </ListItemIcon>
          <ListItemText primary="Donate" />
        </ListItemButton>
      </ListItem>
      {/* <ListItem>
        <ListItemButton href={'/about'} className="profile-popover-link" component={Link}>
          <ListItemIcon>
            <InfoIcon />
          </ListItemIcon>
          <ListItemText primary="About" />
        </ListItemButton>
      </ListItem> */}
      <ListItem>
        <ListItemButton
          href={'https://form.asana.com/?k=4h9ZTRkVUT9ZwfJrmvxDDw&d=1208267282546207'}
          className="profile-popover-link"
          component="a"
          target="_blank"
        >
          <ListItemIcon>
            <AssignmentIcon />
          </ListItemIcon>
          <ListItemText primary="Feedback" />
        </ListItemButton>
      </ListItem>
    </List>
  );
};

const AuthButton = () => {
  const isLoggedIn = useIsLoggedIn();

  return (
    <List className="profile-popover-links">
      {isLoggedIn ? (
        <ListItem>
          <ListItemButton href={'/planner/api/users/auth/logout'} className="profile-popover-link" component="a">
            <ListItemIcon>
              <LogoutIcon />
            </ListItemIcon>
            <ListItemText primary="Log Out" />
          </ListItemButton>
        </ListItem>
      ) : (
        <ListItem>
          <ListItemButton href={'/planner/api/users/auth/google'} className="profile-popover-link" component="a">
            <ListItemIcon>
              <AccountCircleIcon />
            </ListItemIcon>
            <ListItemText primary="Sign In" />
          </ListItemButton>
        </ListItem>
      )}
    </List>
  );
};

const Profile = () => {
  const [anchorEl, setAnchorEl] = useState<HTMLElement | null>(null);
  const open = !!anchorEl;

  const handleClose = () => setAnchorEl(null);

  const handleOpen = (event: React.MouseEvent<HTMLElement>) => {
    setAnchorEl(event.currentTarget);
  };

  const user = useAppSelector((state) => state.user.user);

  return (
    <div>
      <ProfileMenuButtons user={user} handleOpen={handleOpen} />
      <Popover
        className="profile-popover"
        open={open}
        anchorEl={anchorEl}
        onClose={handleClose}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'right' }}
        transformOrigin={{ vertical: 'top', horizontal: 'right' }}
      >
        <div>
          <UserInformation user={user} />
          <ProfileThemeMenu />
          <Divider />
          <ProfileMenuLinks handleLinkClick={handleClose} />
          <Divider />
          <DonateAboutFeedbackButtons />
          <Divider />
          <AuthButton />
        </div>
      </Popover>
    </div>
  );
};

export default Profile;
