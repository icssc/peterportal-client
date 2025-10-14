import './RequirementsListSelector.scss';
import { FC } from 'react';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { RequirementsTabName, setSelectedTab } from '../../../store/slices/courseRequirementsSlice';
import SelectorGroup from '../../../component/SelectorGroup/SelectorGroup';

const tabs = [
  {
    name: 'Major',
  },
  {
    name: 'Minor',
  },
  {
    name: 'GE',
  },
  {
    name: 'Search',
  },
];
const RequirementsListSelector: FC = () => {
  const selectedTab = useAppSelector((state) => state.courseRequirements.selectedTab);
  const dispatch = useAppDispatch();
  const selectTab = (text: string) => dispatch(setSelectedTab(text as RequirementsTabName));

  return (
    <SelectorGroup
      tabList={tabs}
      selectedTab={selectedTab}
      selectTab={selectTab}
      className="requirements-list-selector"
    ></SelectorGroup>
  );
};

export default RequirementsListSelector;
