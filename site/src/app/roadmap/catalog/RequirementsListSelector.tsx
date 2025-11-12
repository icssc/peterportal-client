import './RequirementsListSelector.scss';
import { FC, useContext } from 'react';
import { Button } from 'react-bootstrap';
import ThemeContext from '../../../style/theme-context';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { RequirementsTabName, setSelectedTab } from '../../../store/slices/courseRequirementsSlice';
import { useIsMobile } from '../../../helpers/util';

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
    <Button variant={variant} onClick={selectTab}>
      <span>{text}</span>
    </Button>
  );
};

const RequirementsListSelector: FC = () => {
  const isMobile = useIsMobile();
  const lastTab = isMobile ? 'Search' : 'Saved';

  return (
    <div className="requirements-list-selector">
      <ListSelector text="Major" />
      <ListSelector text="Minor" />
      <ListSelector text="GE" />
      <ListSelector text={lastTab} /> {/** @todo resolve merge conflict with MUI sidebar */}
    </div>
  );
};

export default RequirementsListSelector;
