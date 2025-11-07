'use client';
import './DesktopRoadmapSidebar.scss';
import { Badge, Tab, Tabs } from '@mui/material';
import { CourseCatalog } from '../catalog/CourseCatalog';
import { useEffect, useState } from 'react';

import CompareArrowsIcon from '@mui/icons-material/CompareArrows';
import FormatListBulletedIcon from '@mui/icons-material/FormatListBulleted';
import SearchIcon from '@mui/icons-material/Search';
import { TransferCreditsMenu } from '../transfers/TransferCreditsMenu';
import SavedAndSearch from '../search/SavedAndSearch';
import { useHasUnreadTransfers } from '../../../hooks/transferCredits';
import { useAppDispatch } from '../../../store/hooks';
import { clearUnreadTransfers } from '../../../store/slices/transferCreditsSlice';

const DesktopRoadmapSidebar = () => {
  const [selectedIndex, setSelectedIndex] = useState(1);
  const [hasSeenCredits, setHasSeenCredits] = useState(false);
  const hasUnreadTransfers = useHasUnreadTransfers();
  const dispatch = useAppDispatch();

  useEffect(() => {
    if (selectedIndex === 0) {
      setHasSeenCredits(true);
    } else {
      if (hasSeenCredits) dispatch(clearUnreadTransfers());
      setHasSeenCredits(false);
    }
  }, [dispatch, hasSeenCredits, selectedIndex]);

  return (
    <div className="roadmap-sidebar">
      <Tabs
        className="sidebar-tabs"
        value={selectedIndex}
        onChange={(_, newValue) => setSelectedIndex(newValue)}
        variant="fullWidth"
      >
        <Tab
          icon={<CompareArrowsIcon />}
          iconPosition="start"
          label={
            <Badge variant="dot" color="error" invisible={!hasUnreadTransfers} className="unread-badge">
              Credits
            </Badge>
          }
        />
        <Tab icon={<FormatListBulletedIcon />} iconPosition="start" label="Catalog" />
        <Tab icon={<SearchIcon />} iconPosition="start" label="Search" />
      </Tabs>

      <div className="sidebar-content">
        {selectedIndex === 0 && <TransferCreditsMenu />}
        {selectedIndex === 1 && <CourseCatalog />}
        {selectedIndex === 2 && <SavedAndSearch />}
      </div>
    </div>
  );
};

export default DesktopRoadmapSidebar;
