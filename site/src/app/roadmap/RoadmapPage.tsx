'use client';
import { FC } from 'react';
import './RoadmapPage.scss';
import Planner from './planner/Planner';
import MobileCourseCatalog from './catalog/MobileCourseCatalog';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import AddCoursePopup from './planner/AddCoursePopup';
import { useIsMobile } from '../../helpers/util';
import CoursePreview from '../../component/CoursePreview/CoursePreview';
import DesktopRoadmapSidebar from './sidebar/DesktopRoadmapSidebar';
import { MobileCreditsMenu } from './transfers/MobileCreditsMenu';
import { setShowToast } from '../../store/slices/roadmapSlice';
import Toast from '../../helpers/toast';
import MobileSearchMenu from '../../component/MobileSearchMenu/MobileSearchMenu';

const RoadmapPage: FC = () => {
  const isMobile = useIsMobile();

  const previewCourseId = useAppSelector((state) => state.coursePreview.courseId);

  const dispatch = useAppDispatch();

  const toastMsg = useAppSelector((state) => state.roadmap.toastMsg);
  const toastSeverity = useAppSelector((state) => state.roadmap.toastSeverity);
  const showToast = useAppSelector((state) => state.roadmap.showToast);
  const showFullscreenSearch = useAppSelector((state) => state.roadmap.showMobileFullscreenSearch);

  const handleClose = () => {
    dispatch(setShowToast(false));
  };

  const fullscreenActive = isMobile && showFullscreenSearch;

  if (fullscreenActive) return <MobileSearchMenu />;

  return (
    <div className="roadmap-page">
      {!isMobile && <DesktopRoadmapSidebar />}

      {/* Mobile Popup Menus */}
      <Toast text={toastMsg} severity={toastSeverity} showToast={showToast} onClose={handleClose} />
      <AddCoursePopup />
      <MobileCourseCatalog />
      <MobileCreditsMenu />

      {/* Main Planner View */}
      <div className={`main-wrapper ${isMobile ? 'mobile' : ''}`}>
        <Planner />
        {previewCourseId && <CoursePreview courseId={previewCourseId} />}
      </div>
    </div>
  );
};

export default RoadmapPage;
