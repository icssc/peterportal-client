'use client';
import { FC, useState } from 'react';
import { pluralize, useIsMobile } from '../../../helpers/util';
import './Header.scss';
import RoadmapMultiplan from './RoadmapMultiplan';
import AddYearPopup from '../planner/AddYearPopup';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { setShowMobileCreditsMenu, clearUnreadTransfers } from '../../../store/slices/transferCreditsSlice';
import SaveIcon from '@mui/icons-material/Save';
import SwapHorizOutlinedIcon from '@mui/icons-material/SwapHorizOutlined';
import { Badge, Button, ButtonGroup, Paper, useMediaQuery } from '@mui/material';
import { useSaveRoadmap } from '../../../hooks/planner';
import { useHasUnreadTransfers } from '../../../hooks/transferCredits';

interface HeaderProps {
  courseCount: number;
  unitCount: number;
  missingPrerequisites: Set<string>;
}

const Header: FC<HeaderProps> = ({ courseCount, unitCount }) => {
  const showTransfers = useAppSelector((state) => state.transferCredits.showMobileCreditsMenu);
  const isMobile = useIsMobile();
  const { handler: saveRoadmap } = useSaveRoadmap();
  const dispatch = useAppDispatch();

  const [saveInProgress, setSaveInProgress] = useState(false);

  const handleSave = () => {
    setSaveInProgress(true);
    saveRoadmap().finally(() => setSaveInProgress(false));
  };

  const toggleTransfers = () => {
    if (showTransfers) {
      // After closing the menu, clear all the unread markers
      dispatch(clearUnreadTransfers());
    }
    dispatch(setShowMobileCreditsMenu(!showTransfers));
  };

  const shrinkButtons = useMediaQuery('(max-width: 900px)');
  const buttonSize = shrinkButtons ? 'xsmall' : 'small';

  const hasUnreadTransfers = useHasUnreadTransfers();

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
        <ButtonGroup>
          <AddYearPopup buttonSize={buttonSize} />
          {isMobile && (
            <Badge color="error" variant="dot" invisible={!hasUnreadTransfers}>
              <Button
                variant="contained"
                color="inherit"
                size={buttonSize}
                disableElevation
                className="header-btn"
                startIcon={<SwapHorizOutlinedIcon />}
                onClick={toggleTransfers}
              >
                Add Credits
              </Button>
            </Badge>
          )}
          <Button
            variant="contained"
            color="inherit"
            size={buttonSize}
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
