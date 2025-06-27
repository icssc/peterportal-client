import { FC } from 'react';
import { Button, ButtonGroup } from 'react-bootstrap';
import './Header.scss';

import RoadmapMultiplan from './RoadmapMultiplan';
import AddYearPopup from './AddYearPopup';
import UnreadDot from '../../component/UnreadDot/UnreadDot';

import { PlannerData } from '../../types/types';
import { useAppSelector } from '../../store/hooks';
import { useTransferredCredits, useToggleTransfers } from '../../hooks/transferCredits';
import { getTotalUnitsFromTransfers } from '../../helpers/transferCredits';
import { pluralize } from '../../helpers/util';

import { selectAllPlans } from '../../store/slices/roadmapSlice';
import { collapseAllPlanners, saveRoadmap } from '../../helpers/planner';
import { useIsLoggedIn } from '../../hooks/isLoggedIn';

import SaveIcon from '@mui/icons-material/Save';
import SwapHorizOutlinedIcon from '@mui/icons-material/SwapHorizOutlined';

interface HeaderProps {
  currentPlanData: PlannerData;
}

const Header: FC<HeaderProps> = ({ currentPlanData }) => {
  const { showTransfersMenu, transferredCourses, userAPExams, uncategorizedCourses } = useAppSelector(
    (state) => state.transferCredits,
  );
  const allPlanData = useAppSelector(selectAllPlans);
  const transferred = useTransferredCredits();
  const setToggleTransfers = useToggleTransfers();
  const isLoggedIn = useIsLoggedIn();

  const allCourses = currentPlanData.flatMap((y) => y.quarters.flatMap((q) => q.courses));
  const courseCount = allCourses.length + transferred.courses.length;
  const unitCount =
    allCourses.reduce((sum, c) => sum + c.minUnits, 0) +
    getTotalUnitsFromTransfers(transferred.courses, transferred.ap, transferred.ge, transferred.other);

  const toggleTransfers = () => setToggleTransfers(showTransfersMenu);

  const hasUnread = (group: { unread?: boolean }[]) => group.some((item) => item.unread);
  const hasUnreadTransfers = hasUnread(transferredCourses) || hasUnread(userAPExams) || hasUnread(uncategorizedCourses);

  const handleSave = async () => {
    const collapsed = collapseAllPlanners(allPlanData);
    saveRoadmap(isLoggedIn, collapsed, true);
  };

  return (
    <div className="header">
      <div className="planner-left">
        <RoadmapMultiplan />
        <div>
          <b>{courseCount}</b> course{pluralize(courseCount)}, <b>{unitCount}</b> unit{pluralize(unitCount)}
        </div>
      </div>
      <div className="planner-right">
        <ButtonGroup>
          <AddYearPopup />
          <Button variant="light" className="header-btn ppc-btn" onClick={toggleTransfers}>
            <SwapHorizOutlinedIcon className="header-icon" />
            Transfer Credits
            <UnreadDot show={hasUnreadTransfers} displayFullNewText={false} />
          </Button>
          <Button variant="light" className="header-btn ppc-btn" onClick={handleSave}>
            <SaveIcon className="header-icon" />
            Save
          </Button>
        </ButtonGroup>
      </div>
    </div>
  );
};

export default Header;
