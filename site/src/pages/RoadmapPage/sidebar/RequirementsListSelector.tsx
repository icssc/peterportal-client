import { FC, useContext } from 'react';
import { Button } from 'react-bootstrap';
import './RequirementsListSelector.scss';

import ThemeContext from '../../../style/theme-context';
import { RequirementsTabName, setSelectedTab } from '../../../store/slices/courseRequirementsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';

interface ListSelectorProps {
  text: RequirementsTabName;
}
const ListSelector: FC<ListSelectorProps> = ({ text }) => {
  const { darkMode } = useContext(ThemeContext);
  const selectedTab = useAppSelector((state) => state.courseRequirements.selectedTab);
  const dispatch = useAppDispatch();

  const selected = selectedTab === text;
  const variant = selected ? 'primary' : darkMode ? 'dark' : 'light';
  const selectTab = () => dispatch(setSelectedTab(text));

  return (
    <Button variant={variant} className="ppc-btn" onClick={selectTab}>
      <span>{text}</span>
    </Button>
  );
};

const RequirementsListSelector: FC = () => {
  return (
    <div className="requirements-list-selector">
      <ListSelector text="Major" />
      <ListSelector text="Minor" />
      <ListSelector text="GE" />
      <ListSelector text="Search" />
    </div>
  );
};

export default RequirementsListSelector;
