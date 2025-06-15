import { FC } from 'react';
import { Button, ButtonGroup } from 'react-bootstrap';
import { pluralize } from '../../helpers/util';
import './Header.scss';
import RoadmapMultiplan from './RoadmapMultiplan';
import AddYearPopup from './AddYearPopup';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setShowTransfersMenu, clearUnreadTransfers } from '../../store/slices/transferCreditsSlice';
import UnreadDot from '../../component/UnreadDot/UnreadDot';

import SaveIcon from '@mui/icons-material/Save';
import SwapHorizOutlinedIcon from '@mui/icons-material/SwapHorizOutlined';

interface HeaderProps {
  courseCount: number;
  unitCount: number;
  missingPrerequisites: Set<string>;
  saveRoadmap: () => void;
}

const Header: FC<HeaderProps> = ({ courseCount, unitCount, saveRoadmap }) => {
  const { showTransfersMenu, transferredCourses, userAPExams, uncategorizedCourses } = useAppSelector(
    (state) => state.transferCredits,
  );
  const dispatch = useAppDispatch();

  const toggleTransfers = () => {
    if (showTransfersMenu) {
      // After closing the menu, clear all the unread markers
      dispatch(clearUnreadTransfers());
    }
    dispatch(setShowTransfersMenu(!showTransfersMenu));
  };

  const hasUnread = (group: { unread?: boolean }[]) => group.some((item) => item.unread);
  const hasUnreadTransfers = hasUnread(transferredCourses) || hasUnread(userAPExams) || hasUnread(uncategorizedCourses);

  return (
    <div className="header">
      <div className="planner-left">
        <RoadmapMultiplan />
        <span id="planner-stats">
          <span id="course-count">{courseCount}</span> course{pluralize(courseCount)},{' '}
          <span id="unit-count">{unitCount}</span> unit{pluralize(unitCount)}
        </span>
      </div>
      <div className="planner-right">
        <ButtonGroup>
          <AddYearPopup />
          <Button variant="light" className="header-btn ppc-btn" onClick={toggleTransfers}>
            <SwapHorizOutlinedIcon className="header-icon" />
            Transfer Credits
            <UnreadDot show={hasUnreadTransfers} displayFullNewText={false} />
          </Button>
          <Button variant="light" className="header-btn ppc-btn" onClick={saveRoadmap}>
            <SaveIcon className="header-icon" />
            Save
          </Button>
        </ButtonGroup>
      </div>
    </div>
  );
};

export default Header;
