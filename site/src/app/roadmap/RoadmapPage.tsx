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
import CoursePreview from '../../component/CoursePreview/CoursePreview';
import MobileSearchMenu from '../../component/MobileSearchMenu/MobileSearchMenu';

const RoadmapPage: FC = () => {
  const showSearch = useAppSelector((state) => state.roadmap.showSearch);
  const showFullscreenSearch = useAppSelector((state) => state.roadmap.showFullscreenSearch);
  const isMobile = useIsMobile();
  const sidebarRef = useRef(null);
  const fullscreenSearchRef = useRef(null);

  const previewCourseId = useAppSelector((state) => state.coursePreview.courseId);

  const fullscreenActive = isMobile && showFullscreenSearch;

  return (
    <div className={`roadmap-page ${fullscreenActive ? 'fullscreen-search-active' : ''}`}>
      <AddCoursePopup />
      <CSSTransition in={!isMobile || showSearch} timeout={500} unmountOnExit nodeRef={sidebarRef}>
        <SearchSidebar sidebarRef={sidebarRef} />
      </CSSTransition>
      <div ref={fullscreenSearchRef} className={`fullscreen-search ${fullscreenActive ? 'visible' : ''}`}>
        <MobileSearchMenu />
      </div>
      {isMobile && <TransferCreditsMenu />}
      <div className={`main-wrapper ${isMobile ? 'mobile' : ''}`} aria-hidden={fullscreenActive}>
        <Planner />
        {previewCourseId && <CoursePreview courseId={previewCourseId} />}
      </div>
    </div>
  );
};

export default RoadmapPage;
