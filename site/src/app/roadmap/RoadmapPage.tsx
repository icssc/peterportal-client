'use client';
import { FC, useRef } from 'react';
import './RoadmapPage.scss';
import Planner from './planner/Planner';
import SearchSidebar from './sidebar/SearchSidebar';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import AddCoursePopup from './planner/AddCoursePopup';
import { useIsMobile } from '../../helpers/util';
import { CSSTransition } from 'react-transition-group';
import TransferCreditsMenu from './transfers/TransferCreditsMenu';
import CoursePreview from '../../component/CoursePreview/CoursePreview';
import { setShowToast } from '../../store/slices/roadmapSlice';

import Toast from '../../helpers/toast';

const RoadmapPage: FC = () => {
  const showSearch = useAppSelector((state) => state.roadmap.showSearch);
  const isMobile = useIsMobile();
  const sidebarRef = useRef(null);

  const previewCourseId = useAppSelector((state) => state.coursePreview.courseId);

  const dispatch = useAppDispatch();

  const toastMsg = useAppSelector((state) => state.roadmap.toastMsg);
  const toastSeverity = useAppSelector((state) => state.roadmap.toastSeverity);
  const showToast = useAppSelector((state) => state.roadmap.showToast);

  const handleClose = () => {
    dispatch(setShowToast(false));
  };

  return (
    <div className="roadmap-page">
      <Toast text={toastMsg} severity={toastSeverity} showToast={showToast} onClose={handleClose} />
      <AddCoursePopup />
      <CSSTransition in={!isMobile || showSearch} timeout={500} unmountOnExit nodeRef={sidebarRef}>
        <SearchSidebar sidebarRef={sidebarRef} />
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
