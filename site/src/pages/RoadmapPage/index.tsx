'use client';
import { FC, useRef } from 'react';
import './index.scss';
import Planner from './Planner';
import SearchSidebar from './SearchSidebar';
import { useAppSelector } from '../../store/hooks';
import AddCoursePopup from './AddCoursePopup';
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
        <div className={`main-wrapper ${isMobile ? 'mobile' : ''}`}>
          <Planner />
        </div>
        <CSSTransition in={!isMobile || showSearch} timeout={500} unmountOnExit nodeRef={sidebarRef}>
          <SearchSidebar sidebarRef={sidebarRef} />
        </CSSTransition>
        {isMobile && <TransferCreditsMenu />}
      </div>
    </>
  );
};

export default RoadmapPage;
