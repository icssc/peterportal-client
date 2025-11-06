'use client';
import { FC, useRef } from 'react';
import './RoadmapPage.scss';
import Planner from './planner/Planner';
import MobileCourseCatalog from './sidebar/catalog/MobileCourseCatalog';
import { useAppSelector } from '../../store/hooks';
import AddCoursePopup from './planner/AddCoursePopup';
import { useIsMobile } from '../../helpers/util';
import { CSSTransition } from 'react-transition-group';
import TransferCreditsMenu from './transfers/TransferCreditsMenu';
import CoursePreview from '../../component/CoursePreview/CoursePreview';
import DesktopRoadmapSidebar from './sidebar/DesktopRoadmapSidebar';

const RoadmapPage: FC = () => {
  const showSearch = useAppSelector((state) => state.roadmap.showSearch);
  const isMobile = useIsMobile();
  const sidebarRef = useRef(null);

  const previewCourseId = useAppSelector((state) => state.coursePreview.courseId);

  return (
    <div className="roadmap-page">
      <AddCoursePopup />
      {!isMobile && <DesktopRoadmapSidebar />}
      {/** @todo update/remove comment later; On mobile, add course shows as a popup */}
      <CSSTransition in={showSearch} timeout={500} unmountOnExit nodeRef={sidebarRef}>
        <MobileCourseCatalog sidebarRef={sidebarRef} />
      </CSSTransition>
      {isMobile && <TransferCreditsMenu />}
      <div className={`main-wrapper ${isMobile ? 'mobile' : ''}`}>
        <Planner />
        {previewCourseId && <CoursePreview courseId={previewCourseId} />}
      </div>
    </div>
  );
};

export default RoadmapPage;
