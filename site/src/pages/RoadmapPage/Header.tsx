import React, { FC, useState } from 'react';
import { Button, ButtonGroup, Overlay, Popover } from 'react-bootstrap';
import { ArrowLeftRight, List, Save, Trash } from 'react-bootstrap-icons';
import { useIsDesktop, useIsMobile } from '../../helpers/util';
import { useAppDispatch } from '../../store/hooks';
import { clearPlanner, setShowTransfer } from '../../store/slices/roadmapSlice';
import './Header.scss';
import Transfer from './Transfer';
import RoadmapMultiplan from './RoadmapMultiplan';

interface HeaderProps {
  courseCount: number;
  unitCount: number;
  missingPrerequisites: Set<string>;
  saveRoadmap: () => void;
}

const Header: FC<HeaderProps> = ({ courseCount, unitCount, saveRoadmap, missingPrerequisites }) => {
  const dispatch = useAppDispatch();
  const [target, setTarget] = useState<HTMLElement | null>(null);
  const [showMenu, setShowMenu] = useState(false);

  const isMobile = useIsMobile();
  const isDesktop = useIsDesktop();

  const buttons = (
    <>
      <Button
        variant={isMobile ? 'primary' : 'light'}
        className={isMobile ? 'my-1' : 'header-btn'}
        onClick={() => {
          setShowMenu(false);
          dispatch(setShowTransfer(true));
        }}
      >
        Transfer Credits
        <ArrowLeftRight className="header-icon" />
      </Button>
      <Button
        variant={isMobile ? 'primary' : 'light'}
        className={isMobile ? 'my-1' : 'header-btn'}
        onClick={saveRoadmap}
      >
        Save
        <Save className="header-icon" />
      </Button>
      <Button
        variant={isMobile ? 'primary' : 'light'}
        className={isMobile ? 'my-1' : 'header-btn'}
        onClick={() => dispatch(clearPlanner())}
      >
        Clear
        <Trash className="header-icon" />
      </Button>
    </>
  );

  const onMenuClick = (event: React.MouseEvent) => {
    setShowMenu(!showMenu);
    setTarget(event.target as HTMLElement);
  };

  return (
    <div className="header">
      <Transfer missingPrereqNames={missingPrerequisites} />
      <div className="planner-left">
        <RoadmapMultiplan />
        <span id="planner-stats">
          Total: <span id="course-count">{courseCount}</span> {courseCount === 1 ? 'course' : 'courses'},{' '}
          <span id="unit-count">{unitCount}</span> {unitCount === 1 ? 'unit' : 'units'}
        </span>
      </div>
      <div className="planner-right">
        {isMobile && (
          <>
            <List className="mx-3" onClick={onMenuClick} />
            <Overlay show={showMenu} target={target} placement="left">
              <Popover id="roadmap-header-buttons">
                <Popover.Content>
                  <div className="d-flex flex-column">{buttons}</div>
                </Popover.Content>
              </Popover>
            </Overlay>
          </>
        )}
        {isDesktop && <ButtonGroup>{buttons}</ButtonGroup>}
      </div>
    </div>
  );
};

export default Header;
