import './RequirementsListSelector.scss';
import { FC } from 'react';
import ToggleButton from '@mui/material/ToggleButton';
import ToggleButtonGroup from '@mui/material/ToggleButtonGroup';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { RequirementsTabName, setSelectedTab } from '../../../store/slices/courseRequirementsSlice';

interface ListSelectorProps {
  text: RequirementsTabName;
}
const ListSelector: FC<ListSelectorProps> = ({ text }) => {
  const selectedTab = useAppSelector((state) => state.courseRequirements.selectedTab);
  const dispatch = useAppDispatch();

  const selected = selectedTab === text;
  const selectTab = () => dispatch(setSelectedTab(text));

  return (
    <ToggleButton value={text} className="ppc-btn" selected={selected} onClick={selectTab}>
      <span>{text}</span>
    </ToggleButton>
  );
};

const RequirementsListSelector: FC = () => {
  return (
    <ToggleButtonGroup className="requirements-list-selector">
      <ListSelector text="Major" />
      <ListSelector text="Minor" />
      <ListSelector text="GE" />
      <ListSelector text="Search" />
    </ToggleButtonGroup>
  );
};

export default RequirementsListSelector;
