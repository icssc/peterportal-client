import './DesktopRoadmapSidebar.scss';
import { Tab, Tabs } from '@mui/material';
import { CourseCatalogContent } from '../catalog/CourseCatalog';
import { useState } from 'react';

import CompareArrowsIcon from '@mui/icons-material/CompareArrows';
import FormatListBulletedIcon from '@mui/icons-material/FormatListBulleted';
import SearchIcon from '@mui/icons-material/Search';
import { TransferMenuContent } from '../transfers/TransferCreditsMenu';
import SavedAndSearch from '../search/SavedAndSearch';
// import { useHasUnreadTransfers } from '../../../hooks/transferCredits';

const DesktopRoadmapSidebar = () => {
  const [selectedIndex, setSelectedIndex] = useState(1);
  // const hasUnreadTransfers = useHasUnreadTransfers()

  return (
    <div className="roadmap-sidebar">
      <Tabs
        className="sidebar-tabs"
        value={selectedIndex}
        onChange={(_, newValue) => setSelectedIndex(newValue)}
        variant="fullWidth"
      >
        <Tab icon={<CompareArrowsIcon />} iconPosition="start" sx={{ minHeight: '66px' }} label="Credits" />
        <Tab icon={<FormatListBulletedIcon />} iconPosition="start" sx={{ minHeight: '66px' }} label="Catalog" />
        <Tab icon={<SearchIcon />} iconPosition="start" sx={{ minHeight: '66px' }} label="Search" />
      </Tabs>

      <div className="sidebar-content">
        {selectedIndex === 0 && <TransferMenuContent />}
        {selectedIndex === 1 && <CourseCatalogContent />}
        {selectedIndex === 2 && <SavedAndSearch />}
      </div>
    </div>
  );
};

export default DesktopRoadmapSidebar;
