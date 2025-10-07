'use client';
import { FC, useRef } from 'react';
import './RoadmapPage.scss';
import Planner from './planner/Planner';
import SearchSidebar from './sidebar/SearchSidebar';
import { useAppSelector } from '../../store/hooks';
import AddCoursePopup from './planner/AddCoursePopup';
import { useIsMobile } from '../../helpers/util';
import { CSSTransition } from 'react-transition-group';
import TransferCreditsMenu from './transfers/TransferCreditsMenu';

const RoadmapPage: FC = () => {
  const showSearch = useAppSelector((state) => state.roadmap.showSearch);
  const isMobile = useIsMobile();
  const sidebarRef = useRef(null);

  return (
    <>
      <div className="roadmap-page">
        <AddCoursePopup />
        <CSSTransition in={!isMobile || showSearch} timeout={500} unmountOnExit nodeRef={sidebarRef}>
          <SearchSidebar sidebarRef={sidebarRef} />
        </CSSTransition>
        {isMobile && <TransferCreditsMenu />}
        <div className={`main-wrapper ${isMobile ? 'mobile' : ''}`}>
          <Planner />
        </div>
      </div>
    </>
  );
};

export default RoadmapPage;
