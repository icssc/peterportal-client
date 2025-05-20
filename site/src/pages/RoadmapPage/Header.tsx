import { FC } from 'react';
import { Button, ButtonGroup } from 'react-bootstrap';
import { ArrowLeftRight, Save } from 'react-bootstrap-icons';
import { pluralize } from '../../helpers/util';
import './Header.scss';
import RoadmapMultiplan from './RoadmapMultiplan';
import AddYearPopup from './AddYearPopup';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setShowTransfersMenu } from '../../store/slices/transferCreditsSlice';

interface HeaderProps {
  courseCount: number;
  unitCount: number;
  missingPrerequisites: Set<string>;
  saveRoadmap: () => void;
}

const Header: FC<HeaderProps> = ({ courseCount, unitCount, saveRoadmap }) => {
  const show = useAppSelector((state) => state.transferCredits.showTransfersMenu);
  const dispatch = useAppDispatch();

  const toggleTransfers = () => dispatch(setShowTransfersMenu(!show));

  return (
    <div className="header">
      <div className="planner-left">
        <RoadmapMultiplan />
        <span id="planner-stats">
          Total: <span id="course-count">{courseCount}</span> course{pluralize(courseCount)},{' '}
          <span id="unit-count">{unitCount}</span> unit{pluralize(unitCount)}
        </span>
      </div>
      <div className="planner-right">
        <ButtonGroup>
          <AddYearPopup />
          <Button variant="light" className="header-btn ppc-btn" onClick={toggleTransfers}>
            Transfer Credits
            <ArrowLeftRight className="header-icon" />
          </Button>
          <Button variant="light" className="header-btn ppc-btn" onClick={saveRoadmap}>
            Save
            <Save className="header-icon" />
          </Button>
        </ButtonGroup>
      </div>
    </div>
  );
};

export default Header;
