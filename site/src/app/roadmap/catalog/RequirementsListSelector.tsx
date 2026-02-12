import { FC } from 'react';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { RequirementsTabName, setSelectedCatalogTab } from '../../../store/slices/courseRequirementsSlice';
import TabSelector from './../sidebar/TabSelector';
import { useIsMobile } from '../../../helpers/util';

const RequirementsListSelector: FC = () => {
  const dispatch = useAppDispatch();
  const isMobile = useIsMobile();
  const selectedTab = useAppSelector((state) => state.courseRequirements.selectedCatalogTab);
  const lastTab = isMobile ? 'Search' : 'Saved';

  const tabs = [
    { value: 'Major', label: 'Major' },
    { value: 'Minor', label: 'Minor' },
    { value: 'GE', label: 'GE' },
    { value: lastTab, label: lastTab },
  ];

  const handleTabChange = (tab: string) => {
    dispatch(setSelectedCatalogTab(tab as RequirementsTabName));
  };

  return (
    <div>
      <TabSelector tabs={tabs} selectedTab={selectedTab} onTabChange={handleTabChange} />
    </div>
  );
};

export default RequirementsListSelector;
