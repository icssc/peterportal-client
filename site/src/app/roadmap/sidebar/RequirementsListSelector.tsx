import { FC } from 'react';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { RequirementsTabName, setSelectedTab } from '../../../store/slices/courseRequirementsSlice';
import TabSelector from './TabSelector';

const RequirementsListSelector: FC = () => {
  const dispatch = useAppDispatch();
  const selectedTab = useAppSelector((state) => state.courseRequirements.selectedTab);

  const tabs = [
    { value: 'Major', label: 'Major' },
    { value: 'Minor', label: 'Minor' },
    { value: 'GE', label: 'GE' },
    { value: 'Search', label: 'Search' },
  ];

  const handleTabChange = (tab: string) => {
    dispatch(setSelectedTab(tab as RequirementsTabName));
  };

  return (
    <div>
      <TabSelector tabs={tabs} selectedTab={selectedTab} onTabChange={handleTabChange} />
    </div>
  );
};

export default RequirementsListSelector;
