import './RequirementsListSelector.scss';
import { FC, useContext } from 'react';
import { Button } from 'react-bootstrap';

import ThemeContext from '../../../style/theme-context';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { RequirementsTabName, setSelectedTab } from '../../../store/slices/courseRequirementsSlice';

const RequirementsListSelector: FC = () => {
  const { darkMode } = useContext(ThemeContext);
  const selectedTab = useAppSelector((state) => state.courseRequirements.selectedTab);
  const dispatch = useAppDispatch();

  const requirementsTabNames: RequirementsTabName[] = ['Major', 'Minor', 'GE', 'Search'];

  return (
    <div className="requirements-list-selector">
      {requirementsTabNames.map((text) => (
        <Button
          key={text}
          variant={selectedTab === text ? 'primary' : darkMode ? 'dark' : 'light'}
          className="ppc-btn"
          onClick={() => dispatch(setSelectedTab(text))}
        >
          {text}
        </Button>
      ))}
    </div>
  );
};

export default RequirementsListSelector;
