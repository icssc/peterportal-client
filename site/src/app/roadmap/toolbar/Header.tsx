'use client';
import { FC, useState } from 'react';
import { pluralize } from '../../../helpers/util';
import './Header.scss';
import RoadmapMultiplan from './RoadmapMultiplan';
import AddYearPopup from '../planner/AddYearPopup';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { setShowTransfersMenu, clearUnreadTransfers } from '../../../store/slices/transferCreditsSlice';
import UnreadDot from '../../../component/UnreadDot/UnreadDot';

import SaveIcon from '@mui/icons-material/Save';
import SwapHorizOutlinedIcon from '@mui/icons-material/SwapHorizOutlined';
import { Button, ButtonGroup, Paper } from '@mui/material';
import { useSaveRoadmap } from '../../../hooks/planner';

interface HeaderProps {
  courseCount: number;
  unitCount: number;
  missingPrerequisites: Set<string>;
}

const Header: FC<HeaderProps> = ({ courseCount, unitCount }) => {
  const saveRoadmap = useSaveRoadmap();
  const showTransfers = useAppSelector((state) => state.transferCredits.showTransfersMenu);
  const dispatch = useAppDispatch();

  const [saveInProgress, setSaveInProgress] = useState(false);

  const handleSave = () => {
    setSaveInProgress(true);
    saveRoadmap(true).finally(() => setSaveInProgress(false));
  };

  const toggleTransfers = () => {
    if (showTransfers) {
      // After closing the menu, clear all the unread markers
      dispatch(clearUnreadTransfers());
    }
    dispatch(setShowTransfersMenu(!showTransfers));
  };

  const transferredCourses = useAppSelector((state) => state.transferCredits.transferredCourses);
  const userAPExams = useAppSelector((state) => state.transferCredits.userAPExams);
  const uncategorizedCourses = useAppSelector((state) => state.transferCredits.uncategorizedCourses);

  const hasUnreadTransfers =
    transferredCourses.some((course) => course.unread) ||
    userAPExams.some((ap) => ap.unread) ||
    uncategorizedCourses.some((course) => course.unread);

  return (
    <Paper className="roadmap-header" variant="outlined">
      <div className="planner-left">
        <RoadmapMultiplan />
        <span id="planner-stats">
          <span id="course-count">{courseCount}</span> course{pluralize(courseCount)},{' '}
          <span id="unit-count">{unitCount}</span> unit{pluralize(unitCount)}
        </span>
      </div>
      <div className="planner-actions">
        <ButtonGroup className="planner-action-buttons">
          <AddYearPopup />
          <Button
            variant="contained"
            color="inherit"
            disableElevation
            className="header-btn"
            startIcon={<SwapHorizOutlinedIcon />}
            onClick={toggleTransfers}
          >
            Transfer Credits
            <UnreadDot show={hasUnreadTransfers} displayFullNewText={false} />
          </Button>
          <Button
            variant="contained"
            color="inherit"
            disableElevation
            className="header-btn"
            startIcon={<SaveIcon />}
            loading={saveInProgress}
            onClick={handleSave}
          >
            Save
          </Button>
        </ButtonGroup>
      </div>
    </Paper>
  );
};

export default Header;
