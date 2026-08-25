'use client';
import './DesktopRoadmapSidebar.scss';
import { Badge, Tab, Tabs } from '@mui/material';
import { CourseCatalog } from '../catalog/CourseCatalog';
import { useEffect, useState } from 'react';

import SwapHorizOutlinedIcon from '@mui/icons-material/SwapHorizOutlined';
import FormatListBulletedIcon from '@mui/icons-material/FormatListBulleted';
import SearchIcon from '@mui/icons-material/Search';
import { TransferCreditsMenu } from '../transfers/TransferCreditsMenu';
import SavedAndSearch from '../search/SavedAndSearch';
import { useHasUnreadTransfers } from '../../../hooks/transferCredits';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { clearUnreadTransfers } from '../../../store/slices/transferCreditsSlice';
import { setSelectedSidebarTab } from '../../../store/slices/roadmapSlice';

const DesktopRoadmapSidebar = () => {
  const selectedIndex = useAppSelector((state) => state.roadmap.selectedSidebarTab);
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
        onChange={(_, newValue) => dispatch(setSelectedSidebarTab(newValue))}
        variant="fullWidth"
      >
        <Tab
          icon={<SwapHorizOutlinedIcon />}
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

      <div className="sidebar-content" id="sidebarScrollContainer">
        <div style={{ display: selectedIndex === 0 ? 'block' : 'none' }}>
          <TransferCreditsMenu />
        </div>
        <div style={{ display: selectedIndex === 1 ? 'block' : 'none' }}>
          <CourseCatalog />
        </div>
        <div style={{ display: selectedIndex === 2 ? 'block' : 'none' }}>
          <SavedAndSearch autoFocusSearch />
        </div>
      </div>
    </div>
  );
};

export default DesktopRoadmapSidebar;
